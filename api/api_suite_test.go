package api_test

import (
	"net"
	"os"
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/api"
	"github.com/cloudway/platform/api/client"
	"github.com/cloudway/platform/api/server"
	"github.com/cloudway/platform/api/server/middleware"
	"github.com/cloudway/platform/api/server/router/applications"
	"github.com/cloudway/platform/api/server/router/plugins"
	"github.com/cloudway/platform/api/server/router/system"
	"github.com/cloudway/platform/auth/userdb"
	br "github.com/cloudway/platform/broker"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/container"
	"golang.org/x/net/context"

	_ "github.com/cloudway/platform/auth/userdb/mongodb"
	_ "github.com/cloudway/platform/scm/mock"
)

func TestAPI(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "API Suite")
}

var (
	broker    *br.Broker
	apiServer *server.Server
	serverURL string
	waitChan  chan error
)

const (
	REPO_ROOT      = "/var/git/api_test"
	TEST_USER      = "api_test@example.com"
	TEST_NAMESPACE = "api_test"
	TEST_PASSWORD  = "api_test"
)

var _ = BeforeSuite(func() {
	var err error

	Ω(config.Initialize()).Should(Succeed())
	config.Set("scm.type", "mock")
	config.Set("scm.url", "file://"+REPO_ROOT)
	config.Set("userdb.url", "mongodb://127.0.0.1:27017/api_test")

	cli, err := container.NewEnvClient()
	Ω(err).ShouldNot(HaveOccurred())

	broker, err = br.New(cli)
	Ω(err).ShouldNot(HaveOccurred())

	apiServer = server.New("/api")

	laddr := "127.0.0.1:0"
	l, err := net.Listen("tcp", laddr)
	Ω(err).ShouldNot(HaveOccurred())
	apiServer.Accept(laddr, l)
	serverURL = "http://" + l.Addr().String() + "/api"

	apiServer.UseMiddleware(middleware.NewVersionMiddleware(broker))
	apiServer.UseMiddleware(middleware.NewAuthMiddleware(broker, "/api"))

	apiServer.InitRouter(
		system.NewRouter(broker),
		plugins.NewRouter(broker),
		applications.NewRouter(broker),
	)

	// Create test user
	user := userdb.BasicUser{
		Name:      TEST_USER,
		Namespace: TEST_NAMESPACE,
	}
	Ω(broker.CreateUser(&user, TEST_PASSWORD)).Should(Succeed())

	// The serve API routine never exists unless an error occurs
	// we need to start it as a goroutine and wait on it so
	// daemon doesn't exit
	waitChan = make(chan error)
	go apiServer.Wait(waitChan)
})

var _ = AfterSuite(func() {
	broker.RemoveUser(TEST_USER)
	os.RemoveAll(REPO_ROOT)

	// Close server and wait for serve API to complete
	apiServer.Close()
	apiErr := <-waitChan
	Ω(apiErr).ShouldNot(HaveOccurred())
})

func NewTestClient(login bool) *client.APIClient {
	headers := map[string]string{"Accept": "application/json"}
	cli, err := client.NewAPIClient(serverURL, "", nil, headers)
	ExpectWithOffset(1, err).NotTo(HaveOccurred())

	if login {
		token, err := cli.Authenticate(context.Background(), TEST_USER, TEST_PASSWORD)
		ExpectWithOffset(1, err).NotTo(HaveOccurred())
		cli.SetToken(token)
	}

	return cli
}

var _ = Describe("Server version", func() {
	It("should return server version information", func() {
		cli := NewTestClient(false)
		version, err := cli.ServerVersion(context.Background())
		Ω(err).ShouldNot(HaveOccurred())

		Ω(version.Version).Should(Equal(api.Version))
		Ω(version.GitCommit).Should(Equal(api.GitCommit))
		Ω(version.BuildTime).Should(Equal(api.BuildTime))
	})
})
