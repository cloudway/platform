#!/bin/bash -e

# Global configuration
domain=${CLOUDWAY_DOMAIN:-example.com}

console_dns=api.$domain
console_url=http://$console_dns

bitbucket_dns=git.$domain
bitbucket_url=http://$bitbucket_dns

cd "$(dirname "$BASH_SOURCE")/.."

# start reverse proxy
docker run -d --name cloudway-proxy --restart=always \
           -p 80:80 \
           -v /var/run/docker.sock:/var/run/docker.sock:ro \
           icloudway/proxy

# generate random password
if ! [ -e hack/bitbucket-password ]; then
    LC_CTYPE=C tr -cd '[:alnum:]' < /dev/urandom | fold -w20 | head -n1 > hack/bitbucket-password
fi

# start bitbucket server
bitbucket_mount=/var/atlassian/application-data/bitbucket
docker run -d --name=cloudway-bitbucket --restart=always \
           -p 7999:7999 \
           -v /var/run/docker.sock:/var/run/docker.sock \
           -v bitbucket:$bitbucket_mount \
           -e VIRTUAL_HOST=$bitbucket_dns \
           -e VIRTUAL_PORT=7990 \
           -e BITBUCKET_URL=$bitbucket_url \
           -e BITBUCKET_PASSWORD="$(< hack/bitbucket-password)" \
           -e BITBUCKET_LICENSE="$(< hack/bitbucket-license)" \
           icloudway/bitbucket-server

# start broker
docker run -d --name=cloudway-broker --restart=always \
           -v /var/run/docker.sock:/var/run/docker.sock:ro \
           -v userdb:/data/db \
           -e VIRTUAL_HOST=$console_dns \
           -e VIRTUAL_PORT=6616 \
           -e CLOUDWAY_DOMAIN=$domain \
           -e CONSOLE_URL=$console_url \
           --link cloudway-bitbucket:bitbucket \
           icloudway/broker

# start SSH server
docker run -d --name=cloudway-sshd --restart=always \
           -p 2200:2200 \
           -v /var/run/docker.sock:/var/run/docker.sock:ro \
           --link cloudway-bitbucket:bitbucket \
           icloudway/sshd
