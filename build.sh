#!/bin/bash -e

PROJECT=github.com/cloudway/platform
LDFLAGS="-ldflags=-w"

go build $LDFLAGS -o $GOPATH/bin/cwman $PROJECT/cmd/cwman
docker run --rm -v $GOPATH:/go golang:1.6 go build $LDFLAGS -o /go/bin/cwctl  $PROJECT/cmd/cwctl
docker run --rm -v $GOPATH:/go golang:1.6 go build $LDFLAGS -o /go/src/$PROJECT/proxy/hipache/cwman $PROJECT/cmd/cwman
