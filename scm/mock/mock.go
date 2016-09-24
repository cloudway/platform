package mock

import (
	"archive/tar"
	"bufio"
	"compress/gzip"
	"context"
	"fmt"
	"io"
	"io/ioutil"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"golang.org/x/crypto/ssh"

	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/pkg/serverlog"
	"github.com/cloudway/platform/scm"
)

type mockSCM struct {
	repositoryRoot string
}

func init() {
	old := scm.New
	scm.New = func() (scm.SCM, error) {
		scmtype := config.Get("scm.type")
		if scmtype != "mock" {
			return old()
		}

		var repoRoot string

		scmurl := config.Get("scm.url")
		if scmurl != "" {
			if u, err := url.Parse(scmurl); err != nil {
				return nil, err
			} else {
				repoRoot = u.Path
			}
		} else {
			repoRoot = "/var/git"
		}

		return mockSCM{repoRoot}, nil
	}
}

func (mock mockSCM) Type() string {
	return "git"
}

func (mock mockSCM) ensureNamespaceExist(namespace string) error {
	dir := filepath.Join(mock.repositoryRoot, namespace)
	st, err := os.Stat(dir)
	if os.IsNotExist(err) || !st.IsDir() {
		return scm.NamespaceNotFoundError(namespace)
	} else {
		return err
	}
}

func (mock mockSCM) ensureNamespaceNotExist(namespace string) error {
	dir := filepath.Join(mock.repositoryRoot, namespace)
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		return nil
	} else if err != nil {
		return err
	} else {
		return scm.NamespaceExistError(namespace)
	}
}

func (mock mockSCM) ensureRepositoryExist(namespace, name string) error {
	if err := mock.ensureNamespaceExist(namespace); err != nil {
		return err
	}

	dir := filepath.Join(mock.repositoryRoot, namespace, name)
	st, err := os.Stat(dir)
	if os.IsNotExist(err) || !st.IsDir() {
		return scm.RepoNotFoundError(name)
	} else {
		return err
	}
}

func (mock mockSCM) ensureRepositoryNotExist(namespace, name string) error {
	if err := mock.ensureNamespaceExist(namespace); err != nil {
		return err
	}

	dir := filepath.Join(mock.repositoryRoot, namespace, name)
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		return nil
	} else if err != nil {
		return err
	} else {
		return scm.RepoExistError(name)
	}
}

func (mock mockSCM) isEmptyRepository(namespace, name string) (bool, error) {
	if err := mock.ensureRepositoryExist(namespace, name); err != nil {
		return false, err
	}

	repodir := filepath.Join(mock.repositoryRoot, namespace, name)
	out, err := NewGitRepo(repodir).Output("count-objects")
	if err != nil {
		return false, err
	}
	return strings.HasPrefix(out, "0 objects"), nil
}

func (mock mockSCM) CreateNamespace(namespace string) error {
	if err := mock.ensureNamespaceNotExist(namespace); err != nil {
		return err
	}

	dir := filepath.Join(mock.repositoryRoot, namespace)
	return os.MkdirAll(dir, 0700)
}

func (mock mockSCM) RemoveNamespace(namespace string) error {
	if err := mock.ensureNamespaceExist(namespace); err != nil {
		return err
	}

	dir := filepath.Join(mock.repositoryRoot, namespace)
	return os.RemoveAll(dir)
}

const postReceiveHook = `#!/bin/bash

if git config cloudway.disablehook 2>/dev/null; then
	exit 0
fi

target_branch=$(git config cloudway.deploy || true)
target_ref=$(git rev-parse --symbolic-full-name $target_branch 2>/dev/null)
: ${target_ref:=/refs/heads/master}

while read oldrev newrev refname; do
	ref=$(git rev-parse --symbolic-full-name $refname 2>/dev/null)
	if [ "$ref" = "$target_ref" ]; then
		git archive --format=tar.gz "$ref" | /usr/bin/cwman deploy %s %s
		exit $?
	fi
done
`

func (mock mockSCM) CreateRepo(namespace, name string, purge bool) error {
	repodir := filepath.Join(mock.repositoryRoot, namespace, name)

	if purge {
		os.RemoveAll(repodir)
	}
	if err := mock.ensureRepositoryNotExist(namespace, name); err != nil {
		return err
	}
	if err := os.Mkdir(repodir, 0700); err != nil {
		return err
	}

	repo := NewGitRepo(repodir)
	if err := repo.InitBare(); err != nil {
		return err
	}

	hook := filepath.Join(repodir, "hooks", "post-receive")
	script := fmt.Sprintf(postReceiveHook, name, namespace)
	return ioutil.WriteFile(hook, []byte(script), 0750)
}

func (mock mockSCM) RemoveRepo(namespace, name string) error {
	if err := mock.ensureRepositoryExist(namespace, name); err != nil {
		return err
	}

	repodir := filepath.Join(mock.repositoryRoot, namespace, name)
	return os.RemoveAll(repodir)
}

func (mock mockSCM) Populate(namespace, name string, payload io.Reader, size int64) error {
	if empty, err := mock.isEmptyRepository(namespace, name); !empty || err != nil {
		return err
	}

	// Create temporary directory and extract payload into it
	tempdir, err := ioutil.TempDir("", "repo")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tempdir)

	if err := archive.ExtractFiles(tempdir, payload); err != nil {
		return err
	}

	// Create the temporary git repository
	repo := NewGitRepo(tempdir)
	if err := repo.Init(); err != nil {
		return err
	}
	if err := repo.Config("user.email", "test@example.com"); err != nil {
		return err
	}
	if err := repo.Config("user.name", "Test User"); err != nil {
		return err
	}
	if err := repo.Run("add", "-f", "."); err != nil {
		return err
	}
	if err := repo.Commit("Creating template"); err != nil {
		return err
	}

	// temporarily disable post-receive hook
	repo.Config("cloudway.disablehook", "1")
	defer repo.Run("config", "--unset", "cloudway.disablehook")

	// Push the temporary repository into destination
	repodir := filepath.Join(mock.repositoryRoot, namespace, name)
	return repo.Run("push", "--mirror", repodir)
}

func (mock mockSCM) PopulateURL(namespace, name string, url string) error {
	if empty, err := mock.isEmptyRepository(namespace, name); !empty || err != nil {
		return err
	}

	// Create temporary directory and clone remote repository into it
	tempdir, err := ioutil.TempDir("", "repo")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tempdir)

	repo := NewGitRepo(tempdir)
	if err := repo.Run("clone", "--bare", "--no-hardlinks", url, "."); err != nil {
		return err
	}

	// temporarily disable post-receive hook
	repo.Config("cloudway.disablehook", "1")
	defer repo.Run("config", "--unset", "cloudway.disablehook")

	// Push the temporary repository into destination
	repodir := filepath.Join(mock.repositoryRoot, namespace, name)
	return repo.Run("push", "--mirror", repodir)
}

func (mock mockSCM) Deploy(namespace, name string, branch string, log *serverlog.ServerLog) (err error) {
	if log == nil {
		log = serverlog.Discard
	}

	empty, err := mock.isEmptyRepository(namespace, name)
	if err != nil {
		return err
	}

	cli, err := container.NewEnvClient()
	if err != nil {
		return err
	}

	// Create temporary repository archive
	repofile, err := ioutil.TempFile("", "repo")
	if err != nil {
		return err
	}
	defer func() {
		repofile.Close()
		os.Remove(repofile.Name())
	}()

	if empty {
		// create empty archive
		zw := gzip.NewWriter(repofile)
		tw := tar.NewWriter(zw)
		if err = tw.Close(); err == nil {
			err = zw.Close()
		}
	} else {
		// run git command to generate an archive file
		repodir := filepath.Join(mock.repositoryRoot, namespace, name)
		repo := NewGitRepo(repodir)

		current, err := mock.getCurrentDeployment(namespace, name, branch)
		if err != nil {
			return err
		} else {
			repo.Config("cloudway.deploy", current.Id)
		}

		err = repo.Run("archive", "--format=tar.gz", "-o", repofile.Name(), current.Id)
	}
	if err != nil {
		return err
	}

	// Deploy the repository archive
	if _, err = repofile.Seek(0, os.SEEK_SET); err != nil {
		return err
	}

	return cli.DeployRepo(context.Background(), name, namespace, repofile, log)
}

const _DEFAULT_BRANCH = "refs/heads/master"

func defaultBranch() *scm.Branch {
	return &scm.Branch{
		Id:        _DEFAULT_BRANCH,
		DisplayId: "master",
		Type:      "BRANCH",
	}
}

func (mock mockSCM) GetDeploymentBranch(namespace, name string) (*scm.Branch, error) {
	if empty, err := mock.isEmptyRepository(namespace, name); empty || err != nil {
		return defaultBranch(), err
	}
	return mock.getCurrentDeployment(namespace, name, "")
}

func (mock mockSCM) GetDeploymentBranches(namespace, name string) ([]*scm.Branch, error) {
	if empty, err := mock.isEmptyRepository(namespace, name); empty || err != nil {
		return nil, err
	}
	return mock.getAllBranches(namespace, name)
}

func (mock mockSCM) getCurrentDeployment(namespace, name, refId string) (*scm.Branch, error) {
	repodir := filepath.Join(mock.repositoryRoot, namespace, name)
	repo := NewGitRepo(repodir)

	if refId == "" {
		refId = repo.GetConfig("cloudway.deploy")
		if refId == "" {
			refId = _DEFAULT_BRANCH
		}
	}

	out, err := repo.Output("rev-parse", "--symbolic-full-name", refId)
	rev := strings.TrimSpace(out)
	if err != nil || rev == "" {
		refId = _DEFAULT_BRANCH
	} else {
		refId = rev
	}

	refs, err := mock.getAllBranches(namespace, name)
	if err != nil {
		return nil, err
	}

	var current *scm.Branch
	for _, ref := range refs {
		if refId == ref.Id {
			current = ref
			break
		}
	}
	if current == nil {
		current = defaultBranch()
	}
	return current, nil
}

func (mock mockSCM) getAllBranches(namespace, name string) ([]*scm.Branch, error) {
	repodir := filepath.Join(mock.repositoryRoot, namespace, name)
	repo := NewGitRepo(repodir)

	branches, err := getGitRefs(repo.Command("branch", "--no-color"), "refs/heads/", "BRANCH")
	if err != nil {
		return nil, err
	}
	tags, err := getGitRefs(repo.Command("tag"), "refs/tags/", "TAG")
	if err != nil {
		return nil, err
	}
	return append(branches, tags...), nil
}

func getGitRefs(cmd *exec.Cmd, refPrefix, refType string) ([]*scm.Branch, error) {
	cmd.Stdout = nil
	output, err := cmd.Output()
	if err != nil {
		return nil, err
	}

	refs := strings.Split(string(output), "\n")
	result := make([]*scm.Branch, 0, len(refs))
	for i := range refs {
		id := refs[i]
		if id == "" {
			continue
		}
		if strings.HasPrefix(id, "  ") || strings.HasPrefix(id, "* ") {
			id = id[2:]
		}
		result = append(result, &scm.Branch{
			Id:        refPrefix + id,
			DisplayId: id,
			Type:      refType,
		})
	}
	return result, nil
}

func (mock mockSCM) AddKey(namespace string, key string) error {
	if err := mock.ensureNamespaceExist(namespace); err != nil {
		return err
	}

	_, _, _, _, err := ssh.ParseAuthorizedKey([]byte(key))
	if err != nil {
		return err
	}

	keyfilename := filepath.Join(mock.repositoryRoot, namespace, "authorized_keys")
	keyfile, err := os.OpenFile(keyfilename, os.O_RDWR|os.O_CREATE, 0644)
	if err != nil {
		return err
	}
	defer keyfile.Close()

	r := bufio.NewReader(keyfile)
	key = strings.TrimSpace(key)
	for {
		line, err := r.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				break
			} else {
				return err
			}
		}
		if strings.TrimSpace(line) == key {
			return fmt.Errorf("The SSH public key already exists: %s", key)
		}
	}

	if _, err = keyfile.Seek(0, os.SEEK_END); err == nil {
		if _, err = keyfile.WriteString(key); err == nil {
			_, err = keyfile.Write([]byte{'\n'})
		}
	}
	return err
}

func (mock mockSCM) RemoveKey(namespace string, key string) (err error) {
	if err := mock.ensureNamespaceExist(namespace); err != nil {
		return err
	}

	var removed bool

	keyfilename := filepath.Join(mock.repositoryRoot, namespace, "authorized_keys")
	if _, err = os.Stat(keyfilename); os.IsNotExist(err) {
		return fmt.Errorf("ssh key not found")
	}

	infile, err := os.Open(keyfilename)
	if err != nil {
		return err
	}
	defer infile.Close()

	outfile, err := ioutil.TempFile(filepath.Join(mock.repositoryRoot, namespace), "key")
	if err != nil {
		return err
	}
	defer func() {
		outfile.Close()
		if err == nil && removed {
			os.Remove(keyfilename)
			os.Rename(outfile.Name(), keyfilename)
		} else {
			os.Remove(outfile.Name())
		}
	}()

	r := bufio.NewReader(infile)
	key = strings.TrimSpace(key)
	for {
		line, err := r.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				break
			} else {
				return err
			}
		}
		if strings.TrimSpace(line) == key {
			removed = true
		} else {
			if _, err = outfile.WriteString(line); err != nil {
				return err
			}
		}
	}

	if removed {
		return nil
	} else {
		return fmt.Errorf("ssh key not found")
	}
}

func (mock mockSCM) ListKeys(namespace string) ([]scm.SSHKey, error) {
	if err := mock.ensureNamespaceExist(namespace); err != nil {
		return nil, err
	}

	var keys []scm.SSHKey

	keyfilename := filepath.Join(mock.repositoryRoot, namespace, "authorized_keys")
	keyfile, err := os.Open(keyfilename)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		} else {
			return nil, err
		}
	}
	defer keyfile.Close()

	r := bufio.NewReader(keyfile)
	for {
		line, err := r.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				break
			} else {
				return nil, err
			}
		}
		_, label, _, _, err := ssh.ParseAuthorizedKey([]byte(line))
		if err == nil {
			keys = append(keys, scm.SSHKey{
				Label: label,
				Text:  strings.TrimSpace(line),
			})
		}
	}

	return keys, nil
}

var _ scm.SCM = mockSCM{}
