package mock_test

import (
	"archive/tar"
	"bytes"
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"encoding/pem"
	"fmt"
	"io/ioutil"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/scm"
	mockscm "github.com/cloudway/platform/scm/mock"
	"golang.org/x/crypto/ssh"
)

func TestSCM(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "SCM Suite")
}

var (
	mock      scm.SCM
	repoRoot  = "/var/git/scm_test"
	sshServer *mockscm.SSHServer
	sshURL    string
	waitChan  chan error
)

var _ = BeforeSuite(func() {
	var err error

	Expect(config.Initialize()).To(Succeed())
	config.Set("scm.type", "mock")
	config.Set("scm.url", "file://"+repoRoot)
	mock, err = scm.New()
	Expect(err).NotTo(HaveOccurred())

	sshServer = mockscm.NewSSHServer(repoRoot)

	laddr := "127.0.0.1:0"
	l, err := net.Listen("tcp", laddr)
	Expect(err).ShouldNot(HaveOccurred())
	sshServer.Accept(l)
	sshURL = "ssh://git@" + l.Addr().String()

	waitChan = make(chan error)
	go sshServer.Wait(waitChan)
})

var _ = AfterSuite(func() {
	sshServer.Close()
	sshErr := <-waitChan
	Expect(sshErr).ShouldNot(HaveOccurred())
})

var _ = Describe("SCM", func() {
	AfterEach(func() {
		os.RemoveAll(repoRoot)
	})

	Describe("Create namespace", func() {
		It("should create a directory", func() {
			Expect(mock.CreateNamespace("demo")).To(Succeed())
			Expect(filepath.Join(repoRoot, "demo")).To(BeADirectory())
		})

		It("should fail when namespace already exists", func() {
			Expect(mock.CreateNamespace("demo")).To(Succeed())
			Expect(mock.CreateNamespace("demo")).NotTo(Succeed())
		})
	})

	Describe("Create repository", func() {
		It("should create a git directory", func() {
			Expect(mock.CreateNamespace("demo")).To(Succeed())
			Expect(mock.CreateRepo("demo", "test")).To(Succeed())
			Expect(filepath.Join(repoRoot, "demo", "test")).To(BeADirectory())
			Expect(filepath.Join(repoRoot, "demo", "test", "config")).To(BeARegularFile())
		})

		It("should fail when repository already exists", func() {
			Expect(mock.CreateNamespace("demo")).To(Succeed())
			Expect(mock.CreateRepo("demo", "test")).To(Succeed())
			Expect(mock.CreateRepo("demo", "test")).NotTo(Succeed())
		})

		It("should fail when namespace does not exists", func() {
			Expect(mock.CreateRepo("demo", "test")).NotTo(Succeed())
		})
	})

	Describe("Populate repository from archive", func() {
		var message = []byte("This is a test file")
		var payload = &bytes.Buffer{}

		BeforeEach(func() {
			tw := tar.NewWriter(payload)

			// Create payload archive that contains one README file
			Expect(tw.WriteHeader(&tar.Header{
				Name: "README",
				Mode: 0755,
				Size: int64(len(message)),
			})).To(Succeed())

			_, err := tw.Write(message)
			Expect(err).NotTo(HaveOccurred())
			Expect(tw.Close()).To(Succeed())

			Expect(mock.CreateNamespace("demo")).To(Succeed())
			Expect(mock.CreateRepo("demo", "test")).To(Succeed())
		})

		It("should create a git repository", func() {
			Expect(mock.Populate("demo", "test", payload, int64(payload.Len()))).To(Succeed())

			// Clone the repository into a temporary directory
			tempdir, err := ioutil.TempDir("", "repo")
			Expect(err).NotTo(HaveOccurred())
			defer os.RemoveAll(tempdir)

			repodir := filepath.Join(repoRoot, "demo", "test")
			repo := mockscm.NewGitRepo(tempdir)
			Expect(repo.Command("clone", repodir, tempdir).Run()).To(Succeed())

			// Compare with test files
			testfile := filepath.Join(tempdir, "README")
			Expect(testfile).To(BeARegularFile())
			contents, err := ioutil.ReadFile(testfile)
			Expect(err).NotTo(HaveOccurred())
			Expect(contents).To(Equal(message))
		})

		It("should success even if repository already populated", func() {
			Expect(mock.Populate("demo", "test", payload, int64(payload.Len()))).To(Succeed())
			Expect(mock.Populate("demo", "test", payload, int64(payload.Len()))).To(Succeed())
		})

		It("should fail if namespace does not exist", func() {
			Expect(mock.Populate("nonexist", "test", payload, int64(payload.Len()))).NotTo(Succeed())
		})

		It("should fail if repository does not exist", func() {
			Expect(mock.Populate("demo", "nonexist", payload, int64(payload.Len()))).NotTo(Succeed())
		})
	})

	Describe("Populate repository from URL", func() {
		var message = []byte("This is a test file")
		var tempdir string

		BeforeEach(func() {
			var err error

			// Create a temporary directory that contains test files
			tempdir, err = ioutil.TempDir("", "repo")
			Expect(err).NotTo(HaveOccurred())
			testfile := filepath.Join(tempdir, "README")
			Expect(ioutil.WriteFile(testfile, message, 0644)).To(Succeed())

			// Initialize the git repository
			repo := mockscm.NewGitRepo(tempdir)
			Expect(repo.Init(false)).To(Succeed())
			Expect(repo.Run("add", "-f", ".")).To(Succeed())
			Expect(repo.Commit("Initial commit")).To(Succeed())

			Expect(mock.CreateNamespace("demo")).To(Succeed())
			Expect(mock.CreateRepo("demo", "test")).To(Succeed())
		})

		AfterEach(func() {
			os.RemoveAll(tempdir)
		})

		It("should create a git repository", func() {
			Expect(mock.PopulateURL("demo", "test", tempdir)).To(Succeed())
		})

		It("should success even if repository already populated", func() {
			Expect(mock.PopulateURL("demo", "test", tempdir)).To(Succeed())
			Expect(mock.PopulateURL("demo", "test", tempdir)).To(Succeed())
		})

		It("should fail if namespace does not exist", func() {
			Expect(mock.PopulateURL("nonexist", "test", tempdir)).NotTo(Succeed())
		})

		It("should fail if repository does not exist", func() {
			Expect(mock.PopulateURL("demo", "nonexist", tempdir)).NotTo(Succeed())
		})
	})

	Describe("Deployment branches", func() {
		Context("with non-empty repository", func() {
			var tempdir string
			var repo mockscm.Git

			BeforeEach(func() {
				var err error

				// Create a temporary git repository and commit
				tempdir, err = ioutil.TempDir("", "repo")
				Expect(err).NotTo(HaveOccurred())
				testfile := filepath.Join(tempdir, "README")
				Expect(ioutil.WriteFile(testfile, []byte("This is a test file"), 0644)).To(Succeed())

				// Initialize the git repository
				repo = mockscm.NewGitRepo(tempdir)
				Expect(repo.Init(false)).To(Succeed())
				Expect(repo.Run("add", "-f", ".")).To(Succeed())
				Expect(repo.Commit("Initial commit")).To(Succeed())
			})

			AfterEach(func() {
				os.RemoveAll(tempdir)
			})

			It("should return all available deployment branches", func() {
				// Create some branches and tags in the git repository
				Expect(repo.Run("branch", "develop")).To(Succeed())
				Expect(repo.Run("branch", "hotfix")).To(Succeed())
				Expect(repo.Run("tag", "v1.0")).To(Succeed())
				Expect(repo.Run("tag", "v1.1")).To(Succeed())

				// Push the local git repository
				Expect(mock.CreateNamespace("demo")).To(Succeed())
				Expect(mock.CreateRepo("demo", "test")).To(Succeed())
				Expect(mock.PopulateURL("demo", "test", tempdir)).To(Succeed())

				// Check to see the branches and tags are returned correctly
				expected := []*scm.Branch{
					{
						Id:        "refs/heads/master",
						DisplayId: "master",
						Type:      "BRANCH",
					},
					{
						Id:        "refs/heads/develop",
						DisplayId: "develop",
						Type:      "BRANCH",
					},
					{
						Id:        "refs/heads/hotfix",
						DisplayId: "hotfix",
						Type:      "BRANCH",
					},
					{
						Id:        "refs/tags/v1.0",
						DisplayId: "v1.0",
						Type:      "TAG",
					},
					{
						Id:        "refs/tags/v1.1",
						DisplayId: "v1.1",
						Type:      "TAG",
					},
				}

				actual, err := mock.GetDeploymentBranches("demo", "test")
				Expect(err).NotTo(HaveOccurred())
				Expect(actual).To(ConsistOf(expected))
			})
		})

		Context("with empty repository", func() {
			It("should return default deployment branch", func() {
				Expect(mock.CreateNamespace("demo")).To(Succeed())
				Expect(mock.CreateRepo("demo", "test")).To(Succeed())

				branch, err := mock.GetDeploymentBranch("demo", "test")
				Expect(err).NotTo(HaveOccurred())
				Expect(branch.Id).To(Equal("refs/heads/master"))

				branches, err := mock.GetDeploymentBranches("demo", "test")
				Expect(err).NotTo(HaveOccurred())
				Expect(branches).To(BeEmpty())
			})
		})
	})

	Describe("SSH key", func() {
		var pub, priv string

		BeforeEach(func() {
			var err error
			pub, priv, err = makeSSHKeyPair()
			Expect(err).NotTo(HaveOccurred())

			Expect(mock.CreateNamespace("demo")).To(Succeed())
		})

		Context("Add", func() {
			It("should success if key does not exist", func() {
				Expect(mock.AddKey("demo", pub)).To(Succeed())
			})

			It("should fail if key already exist", func() {
				Expect(mock.AddKey("demo", pub)).To(Succeed())
				Expect(mock.AddKey("demo", pub)).NotTo(Succeed())
			})

			It("should fail if the key is invalid", func() {
				Expect(mock.AddKey("demo", priv)).NotTo(Succeed())
			})

			It("should fail if the namespace does not exists", func() {
				Expect(mock.AddKey("nonexist", pub)).NotTo(Succeed())
			})
		})

		Context("Remove", func() {
			It("should success if key exists", func() {
				Expect(mock.AddKey("demo", pub)).To(Succeed())
				Expect(mock.RemoveKey("demo", pub)).To(Succeed())
			})

			It("should fail if key does not exists", func() {
				Expect(mock.RemoveKey("demo", pub)).NotTo(Succeed())
			})

			It("should fail if namespace does not exists", func() {
				Expect(mock.RemoveKey("nonexist", pub)).NotTo(Succeed())
			})
		})

		Context("List", func() {
			It("should get list of previously added keys", func() {
				Expect(mock.AddKey("demo", pub)).To(Succeed())
				keys, err := mock.ListKeys("demo")
				Expect(err).NotTo(HaveOccurred())
				Expect(keys).To(HaveLen(1))
				Expect(keys[0].Text).To(Equal(strings.TrimSpace(pub)))
			})

			It("should get empty list if no keys added", func() {
				keys, err := mock.ListKeys("demo")
				Expect(err).NotTo(HaveOccurred())
				Expect(keys).To(BeEmpty())
			})

			It("should get empty list if keys removed", func() {
				Expect(mock.AddKey("demo", pub)).To(Succeed())
				Expect(mock.RemoveKey("demo", pub)).To(Succeed())

				keys, err := mock.ListKeys("demo")
				Expect(err).NotTo(HaveOccurred())
				Expect(keys).To(BeEmpty())
			})

			It("should fail if namespace does not exists", func() {
				_, err := mock.ListKeys("nonexist")
				Expect(err).To(HaveOccurred())
			})
		})
	})

	Describe("SSH server", func() {
		var initRepo = func(namespace, name string) (repourl, pub, priv string) {
			var err error

			pub, priv, err = makeSSHKeyPair()
			ExpectWithOffset(1, err).NotTo(HaveOccurred())

			ExpectWithOffset(1, mock.CreateNamespace(namespace)).To(Succeed())
			ExpectWithOffset(1, mock.AddKey(namespace, pub)).To(Succeed())
			ExpectWithOffset(1, mock.CreateRepo(namespace, name)).To(Succeed())

			repourl = sshURL + "/" + namespace + "/" + name + ".git"
			return
		}

		var saveKey = func(key string) (keyfile, wrapper string) {
			var tempfile *os.File
			var err error

			tempfile, err = ioutil.TempFile("", "tmp")
			ExpectWithOffset(1, err).NotTo(HaveOccurred())
			tempfile.Close()

			keyfile = tempfile.Name()
			ExpectWithOffset(1, ioutil.WriteFile(keyfile, []byte(key), 0600)).To(Succeed())

			tempfile, err = ioutil.TempFile("", "tmp")
			ExpectWithOffset(1, err).NotTo(HaveOccurred())
			tempfile.Close()

			wrapper = tempfile.Name()
			script := fmt.Sprintf("#!/bin/sh\nssh -i \"%s\" -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null $@", keyfile)
			ExpectWithOffset(1, ioutil.WriteFile(wrapper, []byte(script), 0755)).To(Succeed())
			os.Chmod(wrapper, 0755)

			return
		}

		var initLocalRepo = func(dir string) {
			testfile := filepath.Join(dir, "testfile")
			git := mockscm.NewGitRepo(dir)

			ExpectWithOffset(1, git.Init(false)).To(Succeed())
			ExpectWithOffset(1, ioutil.WriteFile(testfile, []byte("this is a test"), 0644)).To(Succeed())
			ExpectWithOffset(1, git.Run("add", ".")).To(Succeed())
			ExpectWithOffset(1, git.Commit("initial commit")).To(Succeed())
		}

		var (
			tempdir string
			repourl string
			pubkey  string
			keyfile string
			wrapper string
		)

		BeforeEach(func() {
			var privkey string
			var err error

			repourl, pubkey, privkey = initRepo("demo", "test")
			keyfile, wrapper = saveKey(privkey)

			tempdir, err = ioutil.TempDir("", "repo")
			Expect(err).NotTo(HaveOccurred())
		})

		AfterEach(func() {
			os.RemoveAll(tempdir)
			os.Remove(keyfile)
			os.Remove(wrapper)
		})

		Context("with ssh key", func() {
			It("should success to clone", func() {
				git := mockscm.NewGitRepo(tempdir)
				cmd := git.Command("clone", repourl, tempdir)
				cmd.Env = append(os.Environ(), "GIT_SSH="+wrapper)
				Expect(cmd.Run()).To(Succeed())
			})

			It("should success to push", func() {
				initLocalRepo(tempdir)

				git := mockscm.NewGitRepo(tempdir)
				cmd := git.Command("push", "--set-upstream", repourl, "master")
				cmd.Env = append(os.Environ(), "GIT_SSH="+wrapper)
				Expect(cmd.Run()).To(Succeed())
			})
		})

		Context("without ssh key", func() {
			BeforeEach(func() {
				Expect(mock.RemoveKey("demo", pubkey)).To(Succeed())
			})

			It("should fail to clone", func() {
				git := mockscm.NewGitRepo(tempdir)
				cmd := git.Command("clone", repourl, tempdir)
				cmd.Env = append(os.Environ(), "GIT_SSH="+wrapper)
				Expect(cmd.Run()).NotTo(Succeed())
			})

			It("should fail to push", func() {
				initLocalRepo(tempdir)

				git := mockscm.NewGitRepo(tempdir)
				cmd := git.Command("push", "--set-upstream", repourl, "master")
				cmd.Env = append(os.Environ(), "GIT_SSH="+wrapper)
				Expect(cmd.Run()).NotTo(Succeed())
			})
		})

		Context("with wrong ssh key", func() {
			var other_repourl string

			BeforeEach(func() {
				other_repourl, _, _ = initRepo("other", "test")
			})

			It("should fail to clone", func() {
				git := mockscm.NewGitRepo(tempdir)
				cmd := git.Command("clone", other_repourl, tempdir)
				cmd.Env = append(os.Environ(), "GIT_SSH="+wrapper)
				Expect(cmd.Run()).NotTo(Succeed())
			})

			It("should fail to push", func() {
				initLocalRepo(tempdir)

				git := mockscm.NewGitRepo(tempdir)
				cmd := git.Command("push", "--set-upstream", other_repourl, "master")
				cmd.Env = append(os.Environ(), "GIT_SSH="+wrapper)
				Expect(cmd.Run()).NotTo(Succeed())
			})
		})
	})
})

func makeSSHKeyPair() (pub, priv string, err error) {
	privateKey, err := rsa.GenerateKey(rand.Reader, 1024)
	if err != nil {
		return
	}

	privateKeyOut := &bytes.Buffer{}
	privateKeyPEM := &pem.Block{
		Type:  "RSA PRIVATE KEY",
		Bytes: x509.MarshalPKCS1PrivateKey(privateKey),
	}
	err = pem.Encode(privateKeyOut, privateKeyPEM)
	if err != nil {
		return
	}
	priv = privateKeyOut.String()

	publicKey, err := ssh.NewPublicKey(&privateKey.PublicKey)
	if err != nil {
		return
	}
	pub = string(ssh.MarshalAuthorizedKey(publicKey))

	return pub, priv, nil
}
