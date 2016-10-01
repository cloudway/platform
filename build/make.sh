#!/usr/bin/env bash
set -e

# This script builds various binary artifacts from a checkout of the cloudway
# source code.
#
# Requirements:
# - The current directory should be a checkout of the cloudway source code.
#   Whatever version is checked out will be built.
# - The VERSION file, at the root of the repository, should exist, and
#   will be used as Cloudway binary version and package version.
# - The hash of the git commit will also be included in the Cloudway binary,
#   with the suffix -unsupported if the repository isn't clean.
# - The script is intended to be run inside the docker container specified
#   in the Dockerfile at the root of the source. In other words:
#   DO NOT CALL THIS SCRIPT DIRECTLY.
# - The right way to call this script is to invoke "make" from your checkout
#   of the Cloudway repository.
#   the makefile will do a "docker build -t cloudway . and then
#   "docker run build/make.sh" in the resulting image.

set -o pipefail

export CLOUDWAY_PKG="github.com/cloudway/platform"
export SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export MAKEDIR="$SCRIPTDIR/make"

# This script should run in a container.
inContainer="AssumeSoInitially"
if [ "$(go env GOHOSTOS)" = 'windows' ]; then
    if [ -z "$FROM_DOCKERFILE" ]; then
        unset inContainer
    fi
else
    if [ "$PWD" != "/go/src/$CLOUDWAY_PKG" ] || [ -z "$CLOUDWAY_CROSSPLATFORMS" ]; then
        unset inContainer
    fi
fi

if [ -z "$inContainer" ]; then
    {
        echo "# WARNING! I don't seems to be running in a Docker container."
        echo "# The result of this command might be an incorrect build, and will not be"
        echo "# officially supported."
        echo "#"
        echo "# Try this instead: make all"
        echo "#"
    } >&2
fi

# List of bundles to create when no argument is passed
DEFAULT_BUNDLES=(
    validate-gofmt
    validate-vet
    binary-client
    binary-server
    binary-sandbox
    cross
    tgz
    test-unit
    cover
)

VERSION=$(< ./VERSION)
if command -v git &> /dev/null && [ -d .git ] && git rev-parse &> /dev/null; then
    GITCOMMIT=$(git rev-parse --short HEAD)
    if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
        GITCOMMIT="$GITCOMMIT-unsupported"
        echo "#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
        echo "# GITCOMMIT = $GITCOMMIT"
        echo "# The version you are building is listed as unsupported because"
        echo "# there are some files in the git repository that are in an uncommitted state."
        echo "# Commit these changes, or add to .gitignore to remove the -unsupported from the version."
        echo "# Here is the current list:"
        git status --porcelain --untracked-files=no
        echo "#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    fi
elif [ "$CLOUDWAY_GITCOMMIT" ]; then
    GITCOMMIT="$CLOUDWAY_GITCOMMIT"
else
    echo >&2 "#error: .git directory missing and CLOUDWAY_GITCOMMIT not specified"
    echo >&2 "  Please either build with the .git directory accessible, or specify the"
    echo >&2 "  exact (--short) commit hash you are building using CLOUDWAY_GITCOMMIT"
    echo >&2 "  future accountability in diagnosing build issues.  Thanks!"
    exit 1
fi

! BUILDTIME=$(date --rfc-3339 ns 2> /dev/null | sed -e 's/ /T/') &> /dev/null
if [ -z $BUILDTIME ]; then
    # If using bash 3.1 which doesn't support --rfc-3339, eg Windows CI
    BUILDTIME=$(date -u)
fi

if [ "$AUTO_GOPATH" ]; then
    rm -rf .gopath
    mkdir -p .gopath/src/"$(dirname "${CLOUDWAY_PKG}")"
    ln -sf ../../../.. .gopath/src/"${CLOUDWAY_PKG}"
    export GOPATH="${PWD}/.gopath"
fi

if [ ! "$GOPATH" ]; then
    echo >&2 'error: missing GOPATH; please see https://golang.org/doc/code.html#GOPATH'
    echo >72 '  alternatively, set AUTO_GOPATH=1'
    exit 1
fi

# Use these flags when compiling the tests and final binary

IAMSTATIC='true'
source "$SCRIPTDIR/make/.go-autogen"
if [ -z "$CLOUDWAY_DEBUG" ]; then
    LDFLAGS='-w'
fi

EXTLDFLAGS_STATIC='-static'
LDFLAGS_STATIC="-extldflags \"$EXTLDFLAGS_STATIC\""

# ORIG_BUILDFLAGS is necessary for the cross target which cannot always build
# with options link -race
ORIG_BUILDFLAGS=( -tags "autogen netgo static_build $CLOUDWAY_BUILDTAGS" )

# When $CLOUDWAY_INCREMENTAL_BINARY is set in the environment, enable incremental
# builds by installing dependent packages to the GOPATH
REBUILD_FLAG="-a"
if [ "$CLOUDWAY_INCREMENTAL_BINARY" ]; then
    REBUILD_FLAG="-i"
fi
ORIG_BUILDFLAGS+=( $REBUILD_FLAG )

BUILDFLAGS=( $BUILDFLAGS "${ORIG_BUILDFLAGS[@]}" )

# Test timeout.
if [ "${DOCKER_ENGINE_GOARCH}" == "arm" ]; then
    : ${TIMEOUT:=10m}
elif [ "${DOCKER_ENGINE_GOARCH}" == "windows" ]; then
    : ${TIMEOUT:=8m}
else
    : ${TIMEOUT:=5m}
fi

# Test coverage
HAVE_GO_TEST_COVER=
if \
    go help testflag | grep -- -cover > /dev/null \
    && go tool -n cover &> /dev/null \
; then
    HAVE_GO_TEST_COVER=1
else
    unset COVER
fi

# a helper to provide ".exe" when it's appropriate
binary_extension() {
    if [ "$(go env GOOS)" = 'windows' ]; then
        echo -n '.exe'
    fi
}

hash_files() {
    while [ $# -gt 0 ]; do
        f="$1"
        shift
        dir="$(dirname "$f")"
        base="$(basename "$f")"
        for hashAlgo in md5 sha256; do
            if command -v "${hashAlgo}sum" &> /dev/null; then
                (
                    cd "$dir"
                    "${hashAlgo}sum" "$base" > "$base.$hashAlgo"
                )
            fi
        done
    done
}

bundle() {
    local bundle="$1"; shift
    echo "---> Making bundle: $(basename "$bundle") (in $DEST)"
    source "$SCRIPTDIR/make/$bundle" "$@"
}

main() {
    # We want this to fail if the bundles already exist and cannot be removed.
    # This is to avoid mixing bundles from different versions of the code.
    mkdir -p bundles
    if [ -e "bundles/$VERSION" ] && [ -z "$KEEPBUNDLE" ]; then
        echo "bundles/$VERSION already exists. Removing."
        rm -rf "bundles/$VERSION" && mkdir "bundles/$VERSION" || exit 1
        echo
    fi

    if [ "$(go env GOHOSTOS)" != 'windows' ]; then
        # Windows and symlinks don't get along well
        rm -f bundles/latest
        ln -sf "$VERSION" bundles/latest
    fi

    if [ $# -lt 1 ]; then
        bundles=(${DEFAULT_BUNDLES[@]})
    else
        bundles=($@)
    fi
    for bundle in ${bundles[@]}; do
        export DEST="bundles/$VERSION/$(basename "$bundle")"
        # Cygdrive paths don't play well with go build -o.
        if [[ "$(uname -s)" == CYGWIN* ]]; then
            export DEST="$(cygpath -mw "$DEST")"
        fi
        mkdir -p "$DEST"
        ABS_DEST="$(cd "$DEST" && pwd -P)"
        bundle "$bundle"
        echo
    done
}

main "$@"
