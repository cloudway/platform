#!/bin/bash -e

domain=${CLOUDWAY_DOMAIN:-example.com}
cd "$(dirname "$BASH_SOURCE")/.."

# start reverse proxy
docker run -d --name cloudway-proxy --restart=always \
           -p 80:80 \
           -v /var/run/docker.sock:/var/run/docker.sock \
           icloudway/proxy

# generate random password
if ! [ -e hack/bitbucket-password ]; then
    LC_CTYPE=C tr -cd '[:alnum:]' < /dev/urandom | fold -w20 | head -n1 > hack/bitbucket-password
fi

# start bitbucket server
bitbucket_mount=/var/atlassian/application-data/bitbucket
if [ -n "$DOCKER_HOST" -a -n "$DOCKER_CERT_PATH" ]; then
    docker run -d --name=cloudway-bitbucket --restart=always \
               -p 7999:7999 \
               -v bitbucket:$bitbucket_mount \
               -e VIRTUAL_HOST=git.$domain \
               -e VIRTUAL_PORT=7990 \
               -e DOCKER_HOST \
               -e DOCKER_CERT_PATH=${bitbucket_mount}/certs \
               -e DOCKER_TLS_VERIFY \
               -e BITBUCKET_URL=http://git.$domain \
               -e BITBUCKET_PASSWORD="$(< hack/bitbucket-password)" \
               -e BITBUCKET_LICENSE="$(< hack/bitbucket-license)" \
               icloudway/bitbucket-server

    # install docker certificates into bitbucket for triggering deployment
    docker exec cloudway-bitbucket mkdir -p "$bitbucket_mount/certs"
    for pem in ca.pem cert.pem key.pem; do
        docker cp "${DOCKER_CERT_PATH}/$pem" "cloudway-bitbucket:$bitbucket_mount/certs/$pem"
    done
    docker exec -u root cloudway-bitbucket chown -R daemon:daemon "$bitbucket_mount/certs"
elif [ -e /var/run/docker.sock ]; then
    docker run -d --name=cloudway-bitbucket --restart=always \
               -p 7999:7999 \
               -v /var/run/docker.sock:/var/run/docker.sock \
               -v bitbucket:$bitbucket_mount \
               -e VIRTUAL_HOST=git.$domain \
               -e VIRTUAL_PORT=7990 \
               -e BITBUCKET_URL=http://git.$domain \
               -e BITBUCKET_PASSWORD="$(< hack/bitbucket-password)" \
               -e BITBUCKET_LICENSE="$(< hack/bitbucket-license)" \
               icloudway/bitbucket-server
    docker exec -u root cloudway-bitbucket chown root:daemon /var/run/docker.sock
else
    echo >&2 "Don't know how to connect to docker"
    exit 1
fi

# start broker
docker run -d --name=cloudway-broker --restart=always \
           -v /var/run/docker.sock:/var/run/docker.sock \
           -v userdb:/data/db \
           -e VIRTUAL_HOST=api.$domain \
           -e VIRTUAL_PORT=6616 \
           -e CLOUDWAY_DOMAIN=$domain \
           --link cloudway-bitbucket:bitbucket \
           icloudway/broker
