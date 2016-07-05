#!/bin/bash -e

cd "$(dirname "$BASH_SOURCE")/.."
version=$(< ./VERSION)

# start reverse proxy
docker run -d --name cloudway-proxy --restart=always \
           -p 80:80 \
           -v /var/run/docker.sock:/var/run/docker.sock \
           icloudway/proxy

# start bitbucket server
bitbucket_mount=/var/atlassian/application-data/bitbucket
docker run -d --name=cloudway-bitbucket --restart=always \
           -p 7999:7999 \
           -v bitbucket:$bitbucket_mount \
           -e VIRTUAL_HOST=git.example.com \
           -e VIRTUAL_PORT=7990 \
           -e DOCKER_HOST \
           -e DOCKER_CERT_PATH=${bitbucket_mount}/certs \
           -e DOCKER_TLS_VERIFY \
           atlassian/bitbucket-server

# install docker certificates into bitbucket for triggering deployment
docker exec cloudway-bitbucket mkdir -p "$bitbucket_mount/certs"
for pem in ca.pem cert.pem key.pem; do
    docker cp "${DOCKER_CERT_PATH}/$pem" "cloudway-bitbucket:$bitbucket_mount/certs/$pem"
done
docker exec -u root cloudway-bitbucket chown -R daemon:daemon "$bitbucket_mount/certs"
docker cp "bundles/$version/binary-server/cwman-$version" cloudway-bitbucket:/usr/bin/cwman

# start broker
docker run -d --name=cloudway-broker --restart=always \
           -v /var/run/docker.sock:/var/run/docker.sock \
           -v userdb:/data/db \
           -e VIRTUAL_HOST=api.example.com \
           -e VIRTUAL_PORT=6616 \
           -e CLOUDWAY_DOMAIN=example.com \
           -e CLOUDWAY_SCM_TYPE=bitbucket \
           -e CLOUDWAY_SCM_URL="http://admin:Lgdw6MLpEjYeKJnf6sBu@git.example.com" \
           -e CLOUDWAY_SCM_CLONE_URL="git clone ssh://git@git.example.com:7999/<namespace>/<repo>.git" \
           icloudway/broker
