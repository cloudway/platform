package api_test

import (
	"context"
	"net/http"
	"net/http/httptest"
	"runtime"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/api"
	"github.com/cloudway/platform/api/server/httputils"
	"github.com/cloudway/platform/api/server/middleware"
)

var _ = Describe("Version", func() {
	var ctx = context.Background()

	Context("Server", func() {
		It("should return server version information", func() {
			cli := NewTestClient()
			version, err := cli.ServerVersion(ctx)
			Ω(err).ShouldNot(HaveOccurred())

			Ω(version.Version).Should(Equal(api.Version))
			Ω(version.GitCommit).Should(Equal(api.GitCommit))
			Ω(version.BuildTime).Should(Equal(api.BuildTime))

			dockerVersion, err := broker.ServerVersion(ctx)
			Ω(err).ShouldNot(HaveOccurred())
			Ω(version.DockerVersion).Should(Equal(dockerVersion.Version))

			Ω(version.Os).Should(Equal(runtime.GOOS))
			Ω(version.Arch).Should(Equal(runtime.GOARCH))
		})
	})

	Context("Client", func() {
		It("should success if client version is equal to the server version", func() {
			cli := NewTestClient()
			cli.UpdateClientVersion(api.Version)
			_, err := cli.ServerVersion(ctx)
			Ω(err).ShouldNot(HaveOccurred())
		})

		It("should success if client version less than server version but greater than min version", func() {
			cli := NewTestClient()
			cli.UpdateClientVersion(api.MinVersion + ".1")
			_, err := cli.ServerVersion(ctx)
			Ω(err).ShouldNot(HaveOccurred())
		})

		It("should fail if client version is greater than server version", func() {
			cli := NewTestClient()
			cli.UpdateClientVersion("1000.0")
			_, err := cli.ServerVersion(ctx)
			Ω(err).Should(HaveHTTPStatus(http.StatusBadRequest))
		})

		It("should fail if client version is less than minimum version", func() {
			cli := NewTestClient()
			cli.UpdateClientVersion("0.1")
			_, err := cli.ServerVersion(ctx)
			Ω(err).Should(HaveHTTPStatus(http.StatusBadRequest))
		})
	})

	Context("Middleware", func() {
		It("should get version from context", func() {
			var versionFromContext string

			var handler = func(w http.ResponseWriter, r *http.Request, vars map[string]string) error {
				versionFromContext = httputils.VersionFromContext(r.Context())
				return nil
			}

			m := middleware.NewVersionMiddleware(broker)
			h := m.WrapHandler(handler)

			req, _ := http.NewRequest("GET", "/test", nil)
			resp := httptest.NewRecorder()
			vars := map[string]string{}

			Ω(h(resp, req, vars)).Should(Succeed())
			Ω(versionFromContext).Should(Equal(api.Version))
		})
	})
})
