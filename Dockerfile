# This file describes the standard way to build Cloudway, using Docker
#
# Usage:
#
# # Assemble the full dev environment. This is slow the first time.
# docker build -t cloudway-dev .
#
# # Mount your source in an interactive container for quick testing:
# docker run -v `pwd`:/go/src/github.com/cloudway/platform --privileged -i -t cloudway-dev bash
#
# # Run the test suite:
# docker run --privileged cloudway-dev build/make.sh test

FROM icloudway/dev:latest

ENV GOPATH /go

RUN go get -u github.com/onsi/ginkgo/ginkgo github.com/onsi/gomega

RUN git clone https://github.com/cloudway/plugins.git \
 && ( cd plugins && ./install.sh ) \
 && rm -rf plugins

# Compile Go for cross compilation
ENV CLOUDWAY_CROSSPLATFORMS \
    linux/386 linux/arm \
    darwin/amd64 \
    freebsd/amd64 freebsd/386 freebsd/arm \
    windows/amd64 windows/386

WORKDIR /go/src/github.com/cloudway/platform

VOLUME /data

# Wrap all commands in the "docker-in-docker" script to allow nested containers
ENTRYPOINT ["build/dind"]

# Upload source
COPY . /go/src/github.com/cloudway/platform
