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

const deployTimeout = "10s"

var _ = Describe("Deploy", func() {
	var user = userdb.BasicUser{
		Name:      TESTUSER,
		Namespace: NAMESPACE,
	}

	var (
		tempdir  string
		testfile string
		checkdir string
		app      *container.Container
	)

	BeforeEach(func() {
		var err error

		tempdir, err = ioutil.TempDir("", "repo")
		Expect(err).NotTo(HaveOccurred())
		testfile = filepath.Join(tempdir, "track")

		checkdir, err = ioutil.TempDir("", "check")
		Expect(err).NotTo(HaveOccurred())

		Expect(broker.CreateUser(&user, "test")).To(Succeed())
		br := broker.NewUserBroker(&user, context.Background())

		// Create the application
		options := container.CreateOptions{Name: "test"}
		tags := []string{"mock"}

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
		os.RemoveAll(checkdir)
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
		if err := archive.ExtractFiles(checkdir, r); err != nil {
			return "", err
		}
		content, err := ioutil.ReadFile(filepath.Join(checkdir, "track"))
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

			EventuallyWithOffset(1, fetchDeployedFile, deployTimeout).Should(Equal(actual))
		}

		It("should success to deploy the given branch", func() {
			By("Clone the application repository")
			repodir := filepath.Join(REPOROOT, NAMESPACE, "test")
			repo := mock.NewGitRepo(tempdir)
			Expect(repo.Run("clone", repodir, tempdir)).To(Succeed())

			By("Create some branches and tags")
			createBranch(repo, "develop")
			createBranch(repo, "hotfix")
			createTag(repo, "v1.0")
			createTag(repo, "v1.1")

			By("Continue work on master branch")
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

	Describe("Push to deploy", func() {
		It("should deploy after git push", func() {
			var (
				repodir = filepath.Join(REPOROOT, NAMESPACE, "test")
				repo    = mock.NewGitRepo(tempdir)
				text    = "changed"
			)

			By("Clone the application repository")
			Expect(repo.Run("clone", repodir, tempdir)).To(Succeed())

			By("Make change to local repository")
			Expect(ioutil.WriteFile(testfile, []byte(text), 0644)).To(Succeed())
			Expect(repo.Run("add", "track")).To(Succeed())
			Expect(repo.Commit("make change")).To(Succeed())

			By("New commit should be deployed")
			Expect(repo.Run("push")).To(Succeed())
			Eventually(fetchDeployedFile, deployTimeout).Should(Equal(text))
		})

		It("should deploy to given branch", func() {
			var (
				repodir = filepath.Join(REPOROOT, NAMESPACE, "test")
				repo    = mock.NewGitRepo(tempdir)
			)

			By("Clone the application repository")
			Expect(repo.Run("clone", repodir, tempdir)).To(Succeed())

			By("Create develop branch")
			Expect(repo.Run("checkout", "-b", "develop")).To(Succeed())
			Expect(ioutil.WriteFile(testfile, []byte("develop"), 0644)).To(Succeed())
			Expect(repo.Run("add", "track")).To(Succeed())
			Expect(repo.Commit("in develop branch")).To(Succeed())

			By("Continue work on master branch")
			Expect(repo.Run("checkout", "master")).To(Succeed())
			Expect(ioutil.WriteFile(testfile, []byte("master"), 0644)).To(Succeed())
			Expect(repo.Run("add", "track")).To(Succeed())
			Expect(repo.Commit("in master branch")).To(Succeed())

			By("Push to the remote repository")
			Expect(repo.Run("push", "--mirror")).To(Succeed())

			By("Current deployment should stay on master branch")
			Eventually(fetchDeployedFile, deployTimeout).Should(Equal("master"))

			By("Switch deployment branch to develop")
			Expect(broker.SCM.Deploy(NAMESPACE, "test", "develop"))
			Eventually(fetchDeployedFile, deployTimeout).Should(Equal("develop"))

			By("Switch local repository to develop branch")
			Expect(repo.Run("checkout", "develop")).To(Succeed())
			Expect(ioutil.WriteFile(testfile, []byte("new feature"), 0644)).To(Succeed())
			Expect(repo.Run("add", "track")).To(Succeed())
			Expect(repo.Commit("develop new feature")).To(Succeed())
			Expect(repo.Run("push", "origin", "develop")).To(Succeed())
			Eventually(fetchDeployedFile, deployTimeout).Should(Equal("new feature"))

			By("Switch local repository to master branch (deployment branch should stay on develop branch)")
			Expect(repo.Run("checkout", "master")).To(Succeed())
			Expect(ioutil.WriteFile(testfile, []byte("new release"), 0644)).To(Succeed())
			Expect(repo.Run("add", "track")).To(Succeed())
			Expect(repo.Commit("create new release")).To(Succeed())
			Expect(repo.Run("push", "origin", "master")).To(Succeed())
			Eventually(fetchDeployedFile, deployTimeout).Should(Equal("new feature"))
		})
	})
})
