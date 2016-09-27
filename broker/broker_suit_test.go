package broker_test

import (
	"os"
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	br "github.com/cloudway/platform/broker"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/container"

	_ "github.com/cloudway/platform/auth/userdb/mongodb"
	_ "github.com/cloudway/platform/scm/mock"
)

func TestBroker(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Broker Suite")
}

var broker *br.Broker

const (
	REPOROOT  = "/var/git/broker_test"
	TESTUSER  = "broker_test@example.com"
	NAMESPACE = "broker_test"
)

var _ = BeforeSuite(func() {
	var err error

	engine, err := container.NewEngine()
	Expect(err).NotTo(HaveOccurred())

	Expect(config.Initialize()).To(Succeed())
	config.Set("scm.type", "mock")
	config.Set("scm.url", "file://"+REPOROOT)
	config.Set("userdb.url", "mongodb://127.0.0.1:27017/broker_test")

	broker, err = br.New(engine)
	Expect(err).NotTo(HaveOccurred())
})

var _ = AfterSuite(func() {
	os.RemoveAll(REPOROOT)
})
