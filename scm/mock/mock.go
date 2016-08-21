package mock

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"net/url"
	"os"
	"path/filepath"
	"strings"

	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/scm"
	"golang.org/x/crypto/ssh"
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
	cmd := NewGitRepo(repodir).Command("count-objects")
	cmd.Stdout = nil

	out, err := cmd.Output()
	if err != nil {
		return false, err
	}
	return strings.HasPrefix(string(out), "0 objects"), nil
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

func (mock mockSCM) CreateRepo(namespace, name string) error {
	if err := mock.ensureRepositoryNotExist(namespace, name); err != nil {
		return err
	}

	repodir := filepath.Join(mock.repositoryRoot, namespace, name)
	if err := os.Mkdir(repodir, 0700); err != nil {
		return err
	}
	return NewGitRepo(repodir).Init(true)
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
	if err := repo.Init(false); err != nil {
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

	// Push the temporary repository into destination
	repodir := filepath.Join(mock.repositoryRoot, namespace, name)
	return repo.Run("push", "--mirror", repodir)
}

func (mock mockSCM) Deploy(namespace, name string, branch string) error {
	return nil // Not yet implemented
}

func (mock mockSCM) GetDeploymentBranch(namespace, name string) (*scm.Branch, error) {
	return nil, errors.New("Not yet implemented")
}

func (mock mockSCM) GetDeploymentBranches(namespace, name string) ([]scm.Branch, error) {
	return nil, errors.New("Not yet implemented")
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
