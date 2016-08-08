#!/bin/bash

if [[ "$TRAVIS_TAG" =~ ^v[0-9.]+$ ]]; then
    DOCKER_TAG=${TRAVIS_TAG:1}
elif [ "$TRAVIS_BRANCH" = "master" ]; then
    DOCKER_TAG=latest
elif [ "$TRAVIS_BRANCH" = "develop" ]; then
    DOCKER_TAG=unstable
else
    exit 0
fi

cat > ~/.dockercfg <<EOF
{
  "https://index.docker.io/v1/": {
    "auth": "${HUB_AUTH}",
    "email": "${HUB_EMAIL}"
  }
}
EOF

cp bundles/latest/tgz/cloudway-broker-*-linux-amd64.tar.gz build/broker/cloudway-broker.tar.gz
docker build -t icloudway/broker:$DOCKER_TAG -f build/broker/Dockerfile build/broker
docker push icloudway/broker:$DOCKER_TAG

cp -L bundles/latest/binary-server/cwman build/proxy/cwman
docker build -t icloudway/proxy:$DOCKER_TAG -f build/proxy/Dockerfile build/proxy
docker push icloudway/proxy:$DOCKER_TAG

cp -L bundles/latest/binary-server/cwman build/bitbucket/cwman
cp scm/bitbucket/hooks/target/repo-deployer-*.jar build/bitbucket
docker build -t icloudway/bitbucket-server -f build/bitbucket/Dockerfile build/bitbucket
docker tag icloudway/bitbucket-server icloudway/bitbucket-server:4.6
docker tag icloudway/bitbucket-server icloudway/bitbucket-server:4.6.3
docker push icloudway/bitbucket-server:latest
docker push icloudway/bitbucket-server:4.6
docker push icloudway/bitbucket-server:4.6.3

cp -L bundles/latest/binary-server/cwman build/sshd/cwman
docker build -t icloudway/sshd:$DOCKER_TAG -f build/sshd/Dockerfile build/sshd
docker push icloudway/sshd:$DOCKER_TAG

cp bundles/latest/tgz/cloudway-broker-*-linux-amd64.tar.gz build/platform/cloudway-broker.tar.gz
cp scm/bitbucket/hooks/target/repo-deployer-*.jar build/platform/repo-deployer.jar
docker build -t icloudway/platform:$DOCKER_TAG -f build/platform/Dockerfile build/platform
docker push icloudway/platform:$DOCKER_TAG
