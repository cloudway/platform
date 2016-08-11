#!/bin/bash -e

domain=example.com
console_url=http://api.$domain
bitbucket_url=http://git.$domain

: ${CLOUDWAY_TAG:=latest}

cd "$(dirname "$BASH_SOURCE")/.."

# generate random password
if [ ! -e hack/bitbucket-password ]; then
    LC_CTYPE=C tr -cd '[:alnum:]' < /dev/urandom | fold -w20 | head -n1 > hack/bitbucket-password
fi

# start all-in-one container
docker run -d --name cloudway-platform --restart=always \
           -e CLOUDWAY_DOMAIN=$domain \
           -e CONSOLE_URL=$console_url \
           -e BITBUCKET_URL=$bitbucket_url \
           -e BITBUCKET_PASSWORD="$(< hack/bitbucket-password)" \
           -e BITBUCKET_LICENSE="$(< hack/bitbucket-license)" \
           -v /var/run/docker.sock:/var/run/docker.sock:ro \
           -v cloudway-data:/data \
           -p 80:80 -p 7999:7999 -p 2200:2200 \
           icloudway/platform:${CLOUDWAY_TAG}
