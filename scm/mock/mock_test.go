package mock_test

import (
	"archive/tar"
	"bytes"
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"encoding/pem"
	"golang.org/x/crypto/ssh"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/scm"
	mockscm "github.com/cloudway/platform/scm/mock"
)

func TestSCM(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "SCM Suite")
}

var _ = Describe("SCM", func() {
	var repoRoot = "/var/git/scm_test"
	var mock scm.SCM

	BeforeEach(func() {
		var err error

		os.Setenv("CLOUDWAY_SCM_TYPE", "mock")
		os.Setenv("CLOUDWAY_SCM_URL", "file://"+repoRoot)

		mock, err = scm.New()
		Expect(err).NotTo(HaveOccurred())
	})

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

		It("should fail if repository already populated", func() {
			Expect(mock.Populate("demo", "test", payload, int64(payload.Len()))).To(Succeed())
			Expect(mock.Populate("demo", "test", payload, int64(payload.Len()))).NotTo(Succeed())
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
			Expect(repo.Config("user.email", "test@example.com")).To(Succeed())
			Expect(repo.Config("user.name", "Test User")).To(Succeed())
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

		It("should fail if repository already populated", func() {
			Expect(mock.PopulateURL("demo", "test", tempdir)).To(Succeed())
			Expect(mock.PopulateURL("demo", "test", tempdir)).NotTo(Succeed())
		})

		It("should fail if namespace does not exist", func() {
			Expect(mock.PopulateURL("nonexist", "test", tempdir)).NotTo(Succeed())
		})

		It("should fail if repository does not exist", func() {
			Expect(mock.PopulateURL("demo", "nonexist", tempdir)).NotTo(Succeed())
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
