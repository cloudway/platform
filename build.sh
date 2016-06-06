#!/bin/bash -e

PROJECT=github.com/cloudway/platform

go install $PROJECT/cmd/cwman
docker run --rm -v $GOPATH:/go golang:1.6 go install $PROJECT/cmd/cwctl
docker run --rm -v $GOPATH:/go golang:1.6 go build -o /go/src/$PROJECT/proxy/hipache/cwman $PROJECT/cmd/cwman
