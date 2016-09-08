#!/bin/bash
set -e

PLUGIN_JAR=repo-deployer.jar
PLUGIN_KEY=com.cloudway.bitbucket.plugins.repo-deployer
REST_URL=http://127.0.0.1:7990/rest/plugins/1.0/
AUTH="-u ${BITBUCKET_USER:-admin}:${BITBUCKET_PASSWORD} --basic"
HASH_FILE="${BITBUCKET_HOME}/repo-deployer.md5"
HASH="$(md5sum $PLUGIN_JAR)"

function install {
    token=$(curl -I $AUTH ${REST_URL} 2>/dev/null | grep '^upm-token:' | tr -d '\r' | cut -d' ' -f2)
    if [ "$?" != "0" -o -z "$token" ]; then
        return 1
    fi
    curl -F plugin=@$PLUGIN_JAR $AUTH "${REST_URL}?token=${token}"
}

if ! [ -e "$HASH_FILE" -a "$HASH" = "$(< $HASH_FILE)" ]; then
    for i in {1..20}; do
        if install; then
            echo "$HASH" > "$HASH_FILE"
            exit 0
        fi
        sleep 30
    done
fi
