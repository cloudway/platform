#!/bin/bash
set -e

if [[ "$TRAVIS_TAG" =~ ^v[0-9.]+$ ]]; then
    DOCKER_TAG=${TRAVIS_TAG:1}
else
    case "$TRAVIS_BRANCH" in
    master)      DOCKER_TAG=latest ;;
    develop)     DOCKER_TAG=unstable ;;
    expermental) DOCKER_TAG=expermental ;;
    *) exit 0
    esac
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

cp -L bundles/latest/binary-server/cwman build/sshd/cwman
docker build -t icloudway/sshd:$DOCKER_TAG -f build/sshd/Dockerfile build/sshd
docker push icloudway/sshd:$DOCKER_TAG

empty_dir=$(mktemp -d)
cp build/bitbucket/Dockerfile.deps $empty_dir
docker build -t bitbucket-server:deps -f $empty_dir/Dockerfile.deps $empty_dir
rm -rf $empty_dir

cp -L bundles/latest/binary-server/cwman build/bitbucket/cwman
cp scm/bitbucket/hooks/target/repo-deployer-*.jar build/bitbucket
docker build -t icloudway/bitbucket-server:$DOCKER_TAG -f build/bitbucket/Dockerfile build/bitbucket
docker push icloudway/bitbucket-server:$DOCKER_TAG

cp bundles/latest/tgz/cloudway-broker-*-linux-amd64.tar.gz build/platform/cloudway-broker.tar.gz
cp scm/bitbucket/hooks/target/repo-deployer-*.jar build/platform/repo-deployer.jar
docker build -t icloudway/platform:$DOCKER_TAG -f build/platform/Dockerfile build/platform
docker push icloudway/platform:$DOCKER_TAG
