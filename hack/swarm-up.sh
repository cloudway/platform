#!/bin/bash -e

domain=${CLOUDWAY_DOMAIN:-example.com}
network=${CLOUDWAY_NETWORK:-cw-net}

# change these to satisfy your environment
proxy_node=master
broker_node=master
bitbucket_node=master

cd "$(dirname "$BASH_SOURCE")/.."

# start reverse proxy
docker run -d --name cloudway-proxy --restart=always \
           --net=$network \
           -e CLOUDWAY_NETWORK=$network \
           -p 80:80 \
           -e DOCKER_HOST \
           -v $DOCKER_CERT_PATH:/certs:ro \
           -e DOCKER_CERT_PATH=/certs \
           -e DOCKER_TLS_VERIFY \
           -e "constraint:node==${proxy_node}" \
           icloudway/proxy

# generate random password
if ! [ -e hack/bitbucket-password ]; then
    LC_CTYPE=C tr -cd '[:alnum:]' < /dev/urandom | fold -w20 | head -n1 > hack/bitbucket-password
fi

# start bitbucket server
bitbucket_mount=/var/atlassian/application-data/bitbucket
bitbucket_password=$(< hack/bitbucket-password)

docker run -d --name=cloudway-bitbucket --restart=always \
           --net=$network \
           -p 7999:7999 \
           -v bitbucket:$bitbucket_mount \
           -e VIRTUAL_HOST=git.$domain \
           -e VIRTUAL_PORT=7990 \
           -e DOCKER_HOST \
           -e DOCKER_CERT_PATH=${bitbucket_mount}/certs \
           -e DOCKER_TLS_VERIFY \
           -e BITBUCKET_URL=http://git.$domain \
           -e BITBUCKET_PASSWORD=$bitbucket_password \
           -e BITBUCKET_LICENSE="$(< hack/bitbucket-license)" \
           -e "constraint:node==${bitbucket_node}" \
           icloudway/bitbucket-server

# install docker certificates into bitbucket for triggering deployment
docker exec cloudway-bitbucket mkdir -p "$bitbucket_mount/certs"
for pem in ca.pem cert.pem key.pem; do
    docker cp "${DOCKER_CERT_PATH}/$pem" "cloudway-bitbucket:$bitbucket_mount/certs/$pem"
done
docker exec -u root cloudway-bitbucket chown -R daemon:daemon "$bitbucket_mount/certs"

# start broker
docker run -d --name=cloudway-broker --restart=always \
           --net=$network \
           -v userdb:/data/db \
           -e CLOUDWAY_NETWORK=$network \
           -e DOCKER_HOST \
           -v $DOCKER_CERT_PATH:/certs:ro \
           -e DOCKER_CERT_PATH=/certs \
           -e DOCKER_TLS_VERIFY \
           -e VIRTUAL_HOST=api.$domain \
           -e VIRTUAL_PORT=6616 \
           -e CLOUDWAY_DOMAIN=$domain \
           -e CLOUDWAY_SCM_TYPE=bitbucket \
           -e CLOUDWAY_SCM_URL=http://admin:${bitbucket_password}@bitbucket.local:7990 \
           -e CLOUDWAY_SCM_CLONE_URL="git clone ssh://git.${domain}:7999/<namespace>/<repo>.git" \
           --link cloudway-bitbucket:bitbucket.local \
           -e "constraint:node==${broker_node}" \
           icloudway/broker

