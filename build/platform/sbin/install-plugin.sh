#!/bin/bash
set -e

PLUGIN_JAR=${BITBUCKET_INSTALL_DIR}/repo-deployer.jar
PLUGIN_KEY=com.cloudway.bitbucket.plugins.repo-deployer
REST_URL=http://127.0.0.1:7990/rest/plugins/1.0/
AUTH="-u ${BITBUCKET_USER:-admin}:${BITBUCKET_PASSWORD} --basic"

function is_installed {
    status=$(curl -I $AUTH ${REST_URL}${PLUGIN_KEY}-key 2>/dev/null | head -n1 | cut -d' ' -f2)
    [ "$?" == "0" -a "$status" == "200" ]
}

function install {
    token=$(curl -I $AUTH ${REST_URL} 2>/dev/null | grep '^upm-token:' | tr -d '\r' | cut -d' ' -f2)
    if [ "$?" != "0" -o -z "$token" ]; then
        return 1
    fi
    curl -F plugin=@$PLUGIN_JAR $AUTH "${REST_URL}?token=${token}"
}

for i in {1..20}; do
    is_installed && exit 0
    install && exit 0
    sleep 30
done
