package broker_test

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/auth/userdb"
	"github.com/cloudway/platform/container"
)

var _ = Describe("Applications", func() {
	var user = userdb.BasicUser{
		Name:      TESTUSER,
		Namespace: NAMESPACE,
	}

	BeforeEach(func() {
		Expect(broker.CreateUser(&user, "test")).To(Succeed())
	})

	AfterEach(func() {
		Expect(broker.RemoveUser(TESTUSER)).To(Succeed())
	})

	Describe("Create", func() {
		It("should create application", func() {
			br := broker.NewUserBroker(&user)

			options := container.CreateOptions{
				Name: "test",
			}
			tags := []string{"php", "mysql"}

			containers, err := br.CreateApplication(options, tags)
			Expect(err).NotTo(HaveOccurred())

			Expect(containers).To(HaveLen(2))
			for _, c := range containers {
				Expect(c.Name).To(Equal("test"))
				Expect(c.Namespace).To(Equal(NAMESPACE))
				if c.Category().IsFramework() {
					Expect(c.PluginTag()).To(HavePrefix("php:"))
				} else {
					Expect(c.PluginTag()).To(HavePrefix("mysql:"))
					Expect(c.ServiceName()).To(Equal("mysql"))
				}
			}

			apps, err := br.GetApplications()
			Expect(err).NotTo(HaveOccurred())
			Expect(apps).To(HaveKey("test"))
			Expect(apps["test"].Plugins).To(ConsistOf(HavePrefix("php:"), HavePrefix("mysql:")))

			Expect(br.RemoveApplication("test")).To(Succeed())
		})
	})
})
