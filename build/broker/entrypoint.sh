#!/bin/bash

: ${CLOUDWAY_DOMAIN:=example.com}
export CLOUDWAY_DOMAIN

if [ "$CLOUDWAY_DOMAIN" != 'example.com' ]; then
    sed -i "s/example\\.com/$CLOUDWAY_DOMAIN/g" /usr/local/cloudway/conf/cloudway.conf
fi

if [ -z "$CLOUDWAY_SCM_URL" -a -n "$BITBUCKET_ENV_BITBUCKET_URL" ]; then
    BITBUCKET_HOST=$BITBUCKET_PORT_7990_TCP_ADDR
    BITBUCKET_PORT=$BITBUCKET_PORT_7990_TCP_PORT
    BITBUCKET_USER=${BITBUCKET_ENV_BITBUCKET_USER:-admin}
    BITBUCKET_PASS=$BITBUCKET_ENV_BITBUCKET_PASSWORD
    BITBUCKET_URL="http://${BITBUCKET_USER}:${BITBUCKET_PASS}@${BITBUCKET_HOST}:${BITBUCKET_PORT}"

    export CLOUDWAY_SCM_TYPE=bitbucket
    export CLOUDWAY_SCM_URL=$BITBUCKET_URL
    export CLOUDWAY_SCM_CLONE_URL="git clone ssh://git@git.${CLOUDWAY_DOMAIN}:7999/<namespace>/<repo>.git"
fi

exec supervisord -c /etc/supervisor/supervisord.conf
