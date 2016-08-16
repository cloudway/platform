.PHONY: all binary build vendor cross default shell test validate help

HOST_OSARCH := $(shell go env GOOS)/$(shell go env GOARCH)

# get OS/Arch of docker engine
DOCKER_OSARCH := $(shell bash -c 'source build/make/.detect-daemon-osarch && echo $${DOCKER_ENGINE_OSARCH:-$$DOCKER_CLIENT_OSARCH}')
DOCKERFILE := $(shell bash -c 'source build/make/.detect-daemon-osarch && echo $${DOCKERFILE}')

# env vars passed through directly to Docker's build scripts
# to allow things link `make KEEPBUNDLE=1 binary` easily
DOCKER_ENVS := \
    -e BUILDFLAGS \
    -e KEEPBUNDLE \
    -e CLOUDWAY_DEBUG \
    -e CLOUDWAY_GITCOMMIT \
    -e CLOUDWAY_INCREMENTAL_BINARY \
    -e TESTDIRS \
    -e TESTFLAGS \
    -e CROSS \
    -e COVER \
    -e TIMEOUT

# to allow `make BIND_DIR=. shell` or `make BIND_DIR= test`
# (default to no bind mount if DOCKER_HOST is set)
BIND_DIR := $(if $(BINDDIR),$(BINDDIR),bundles)
DOCKER_MOUNT := $(if $(BIND_DIR),-v "$(CURDIR)/$(BIND_DIR):/go/src/github.com/cloudway/platform/$(BIND_DIR)")
VENDOR_MOUNT := -v "$(CURDIR)/vendor:/go/src/github.com/cloudway/platform/vendor"

GIT_BRANCH := $(shell git rev-parse --abbrev-ref HEAD 2>/dev/null)
GIT_BRANCH_CLEAN := $(shell echo $(GIT_BRANCH) | sed -e "s/[^[:alnum:]]/-/g")
DOCKER_IMAGE := cloudway-dev$(if $(GIT_BRANCH_CLEAN),:$(GIT_BRANCH_CLEAN))

DOCKER_FLAGS := docker run --rm -i --privileged $(DOCKER_ENVS) $(DOCKER_MOUNT)

# if this session isn't interactive, then we don't want to allocate a
# TTY, which would fail, but if it is interactive, we do want to attach
# so that the user can send e.g. ^C through.
INTERACTIVE := $(shell [ -t 0 ] && echo 1 || echo 0)
ifeq ($(INTERACTIVE), 1)
	DOCKER_FLAGS += -t
endif

DOCKER_RUN_DOCKER := $(DOCKER_FLAGS) "$(DOCKER_IMAGE)"
DOCKER_RUN_VENDOR := $(DOCKER_FLAGS) $(VENDOR_MOUNT) "$(DOCKER_IMAGE)"

default: build
	CROSS=$(HOST_OSARCH) $(DOCKER_RUN_DOCKER) build/make.sh validate-vet binary cross

all: build ## validate all checks, build linux binaries, run all test\ncross build non-linux binaries and generate archives
	$(DOCKER_RUN_DOCKER) build/make.sh

binary: build ## build the linux binaries
	$(DOCKER_RUN_DOCKER) build/make.sh binary

build: bundles
	docker build ${DOCKER_BUILD_ARGS} -t "$(DOCKER_IMAGE)" -f "$(DOCKERFILE)" .

vendor: build ## update vendored dependencies
	$(DOCKER_RUN_VENDOR) build/vendor.sh

bundles:
	mkdir bundles

cross: build ## cross build the binaries for darwin, freebsd and windows
	$(DOCKER_RUN_DOCKER) build/make.sh binary cross

tgz: build ## build the archive containing the binaries
	$(DOCKER_RUN_DOCKER) build/make.sh binary cross tgz

shell: build ## start a shell inside the build env
	$(DOCKER_RUN_DOCKER) bash

test: build ## run the tests
	$(DOCKER_RUN_DOCKER) build/make.sh test-unit cover

validate: build ## validate go vet
	$(DOCKER_RUN_DOCKER) build/make.sh validate-lint validate-vet

help: ## this help
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {sub("\\\\n",sprintf("\n%21c"," "), $$2);printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)
