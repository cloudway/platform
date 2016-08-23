package broker_test

import (
	"io/ioutil"
	"os"
	"path/filepath"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/auth/userdb"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/scm"
	"github.com/cloudway/platform/scm/mock"
	"golang.org/x/net/context"
)

var _ = Describe("Deploy", func() {
	var user = userdb.BasicUser{
		Name:      TESTUSER,
		Namespace: NAMESPACE,
	}

	var (
		tempdir  string
		testfile string
		app      *container.Container
	)

	BeforeEach(func() {
		var err error

		tempdir, err = ioutil.TempDir("", "repo")
		Expect(err).NotTo(HaveOccurred())
		testfile = filepath.Join(tempdir, "track")

		Expect(broker.CreateUser(&user, "test")).To(Succeed())
		br := broker.NewUserBroker(&user, context.Background())

		// Create the application
		options := container.CreateOptions{Name: "test"}
		tags := []string{"php"}

		containers, err := br.CreateApplication(options, tags)
		Expect(err).NotTo(HaveOccurred())
		Expect(containers).To(HaveLen(1))
		app = containers[0]

		Expect(br.StartApplication("test")).To(Succeed())
	})

	AfterEach(func() {
		br := broker.NewUserBroker(&user, context.Background())
		br.RemoveApplication("test")
		broker.RemoveUser(TESTUSER)
		os.RemoveAll(tempdir)
	})

	var createBranch = func(repo mock.Git, branch string) {
		ExpectWithOffset(1, ioutil.WriteFile(testfile, []byte(branch), 0644)).To(Succeed())
		ExpectWithOffset(1, repo.Run("add", "track")).To(Succeed())
		ExpectWithOffset(1, repo.Commit("in "+branch+" branch")).To(Succeed())
		ExpectWithOffset(1, repo.Run("branch", branch)).To(Succeed())
	}

	var createTag = func(repo mock.Git, tag string) {
		ExpectWithOffset(1, ioutil.WriteFile(testfile, []byte(tag), 0644)).To(Succeed())
		ExpectWithOffset(1, repo.Run("add", "track")).To(Succeed())
		ExpectWithOffset(1, repo.Commit("tagged with "+tag)).To(Succeed())
		ExpectWithOffset(1, repo.Run("tag", tag)).To(Succeed())
	}

	var fetchDeployedFile = func() (string, error) {
		r, _, err := broker.CopyFromContainer(context.Background(), app.ID, app.RepoDir()+"/track")
		if err != nil {
			return "", err
		}
		defer r.Close()
		if err := archive.ExtractFiles(tempdir, r); err != nil {
			return "", err
		}
		content, err := ioutil.ReadFile(testfile)
		return string(content), err
	}

	Describe("Manual deploy", func() {
		var expectedBranches = []*scm.Branch{
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

		var assertDeployment = func(branch, actual string) {
			ExpectWithOffset(1, broker.SCM.Deploy(NAMESPACE, "test", branch)).To(Succeed())

			ref, err := broker.SCM.GetDeploymentBranch(NAMESPACE, "test")
			ExpectWithOffset(1, err).NotTo(HaveOccurred())
			ExpectWithOffset(1, ref.DisplayId).To(Equal(actual))

			EventuallyWithOffset(1, fetchDeployedFile, "5s").Should(Equal(actual))
		}

		It("should success to deploy the given branch", func() {
			By("Clone the application repository")
			repodir := filepath.Join(REPOROOT, NAMESPACE, "test")
			repo := mock.NewGitRepo(tempdir)
			Expect(repo.Run("clone", repodir, tempdir)).To(Succeed())
			Expect(repo.Config("user.email", "test@example.com")).To(Succeed())
			Expect(repo.Config("user.name", "Test User")).To(Succeed())

			By("Create some branches and tags")
			createBranch(repo, "develop")
			createBranch(repo, "hotfix")
			createTag(repo, "v1.0")
			createTag(repo, "v1.1")

			By("Continue with master branch")
			Expect(ioutil.WriteFile(testfile, []byte("master"), 0644)).To(Succeed())
			Expect(repo.Run("add", "track")).To(Succeed())
			Expect(repo.Commit("in master branch")).To(Succeed())

			By("Push to the remote repository")
			Expect(repo.Run("push", "--mirror")).To(Succeed())

			By("Ensure branches created in remote repository")
			actualBranches, err := broker.SCM.GetDeploymentBranches(NAMESPACE, "test")
			Expect(err).NotTo(HaveOccurred())
			Expect(actualBranches).To(ConsistOf(expectedBranches))

			By("Deploy to given branch")
			assertDeployment("master", "master")
			assertDeployment("develop", "develop")
			assertDeployment("hotfix", "hotfix")
			assertDeployment("v1.0", "v1.0")
			assertDeployment("v1.1", "v1.1")

			By("Deploy to a non-existing branch will reset to default branch")
			assertDeployment("no-such-branch", "master")
		})
	})

	PDescribe("Push to deploy", func() {
		It("should deploy after git push", func() {
			var text = "changed"

			By("Clone the application repository")
			repodir := filepath.Join(REPOROOT, NAMESPACE, "test")
			repo := mock.NewGitRepo(tempdir)
			Expect(repo.Run("clone", repodir, tempdir)).To(Succeed())
			Expect(repo.Config("user.email", "test@example.com")).To(Succeed())
			Expect(repo.Config("user.name", "Test User")).To(Succeed())

			By("Make change to local repository")
			Expect(ioutil.WriteFile(testfile, []byte(text), 0644)).To(Succeed())
			Expect(repo.Run("add", "track")).To(Succeed())
			Expect(repo.Commit("make change")).To(Succeed())
			Expect(repo.Run("push")).To(Succeed())

			By("New commit should be deployed")
			Eventually(fetchDeployedFile, "5s").Should(Equal(text))
		})
	})
})
