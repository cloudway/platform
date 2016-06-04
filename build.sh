go install github.com/cloudway/platform/cmd/cwman
docker run --rm -v $GOPATH:/go golang:1.6 go install github.com/cloudway/platform/cmd/cwctl

