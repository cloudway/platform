#!/bin/bash -e

PROJECT=github.com/cloudway/platform
IMAGE_TAG=cloudway/hipache-proxy
BUILD_CONTEXT=$GOPATH/src/$PROJECT/proxy/hipache

docker rm -f proxy || true
docker rmi -f $IMAGE_TAG || true

docker build -t $IMAGE_TAG -f $BUILD_CONTEXT/Dockerfile $BUILD_CONTEXT
docker run -d --name proxy -v /var/run/docker.sock:/var/run/docker.sock:ro -p 80:80 --restart=always $IMAGE_TAG
