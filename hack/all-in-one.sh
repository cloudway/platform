#!/bin/bash -e

DOMAIN=example.com
CONSOLE_URL=http://api.$domain

: ${CLOUDWAY_TAG:=latest}

cd "$(dirname "$BASH_SOURCE")/.."

# start all-in-one container
docker run -d --name cloudway-platform --restart=always \
           -e CLOUDWAY_DOMAIN=$DOMAIN \
           -e CONSOLE_URL=$CONSOLE_URL \
           -v /var/run/docker.sock:/var/run/docker.sock:ro \
           -v cloudway-data:/data \
           -p 80:80 -p 7999:7999 -p 2200:2200 \
           icloudway/platform:${CLOUDWAY_TAG}
