package api_test

import (
	"context"
	"fmt"
	"net"
	"os"
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	"github.com/onsi/gomega/format"

	"github.com/cloudway/platform/api/client"
	"github.com/cloudway/platform/api/server"
	"github.com/cloudway/platform/api/server/middleware"
	"github.com/cloudway/platform/api/server/router/applications"
	"github.com/cloudway/platform/api/server/router/namespace"
	"github.com/cloudway/platform/api/server/router/plugins"
	"github.com/cloudway/platform/api/server/router/system"
	"github.com/cloudway/platform/auth/userdb"
	br "github.com/cloudway/platform/broker"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/pkg/rest"

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

	engine, err := container.NewEngine()
	Ω(err).ShouldNot(HaveOccurred())

	broker, err = br.New(engine)
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
		namespace.NewRouter(broker),
		applications.NewRouter(broker),
	)

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

type TestClient struct {
	*client.APIClient
	user *userdb.BasicUser
}

func NewTestClient() *TestClient {
	headers := map[string]string{"Accept": "application/json"}
	cli, err := client.NewAPIClient(serverURL, "", nil, headers)
	ExpectWithOffset(1, err).NotTo(HaveOccurred())
	return &TestClient{APIClient: cli}
}

func NewTestClientWithUser(login bool) *TestClient {
	return makeClient(TEST_USER, "", login)
}

func NewTestClientWithNamespace(login bool) *TestClient {
	return makeClient(TEST_USER, TEST_NAMESPACE, login)
}

func makeClient(username, namespace string, login bool) *TestClient {
	cli := NewTestClient()

	cli.user = &userdb.BasicUser{Name: username, Namespace: namespace}
	ExpectWithOffset(2, broker.CreateUser(cli.user, TEST_PASSWORD)).To(Succeed())

	if login {
		token, err := cli.Authenticate(context.Background(), username, TEST_PASSWORD)
		ExpectWithOffset(2, err).NotTo(HaveOccurred())
		cli.SetToken(token)
	}

	return cli
}

func (cli *TestClient) NewUserBroker() *br.UserBroker {
	return broker.NewUserBroker(&*cli.user, context.Background())
}

func (cli *TestClient) Close() {
	if cli != nil && cli.user != nil {
		ExpectWithOffset(1, broker.RemoveUser(TEST_USER)).To(Succeed())
	}
}

type HaveHTTPStatus int

func (matcher HaveHTTPStatus) Match(actual interface{}) (success bool, err error) {
	if actual == nil {
		return false, fmt.Errorf("Expected an error, got nil")
	}

	if _, ok := actual.(error); !ok {
		return false, fmt.Errorf("Expected an error. Got:\n%s", format.Object(actual, 1))
	}

	se, ok := actual.(rest.ServerError)
	return ok && se.StatusCode() == int(matcher), nil
}

func (matcher HaveHTTPStatus) FailureMessage(actual interface{}) (message string) {
	return format.Message(actual, fmt.Sprintf("to have HTTP status code %d", int(matcher)))
}

func (matcher HaveHTTPStatus) NegatedFailureMessage(actual interface{}) (message string) {
	return format.Message(actual, fmt.Sprintf("not to have HTTP status code %d", int(matcher)))
}
