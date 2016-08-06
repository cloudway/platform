#!/bin/bash
set -e

# allow the container to be started with `--user`
mkdir -p $BITBUCKET_HOME
chown ${RUN_USER}:${RUN_GROUP} $BITBUCKET_HOME

# https://jira.atlassian.com/browse/BSERV-8345
sed -i $BITBUCKET_INSTALL_DIR/bin/setenv.sh \
    -e 's|JVM_SUPPORT_RECOMMENDED_ARGS=""|JVM_SUPPORT_RECOMMENDED_ARGS="-Djava.security.egd=file:/dev/./urandom"|'

UNATTENDED_CONFIG=$BITBUCKET_HOME/shared/bitbucket.properties
if ! [ -e $UNATTENDED_CONFIG ]; then
    if [ -z "$BITBUCKET_URL" -o -z "$BITBUCKET_PASSWORD" -o -z "$BITBUCKET_LICENSE" ]; then
        echo >&2 'You must specifiy the BITBUCKET_URL, BITBUCKET_PASSWORD and BITBUCKET_LICENSE environment variables'
        exit 1
    fi

    # Use the \u000a character to not break the license over multiple lines
    BITBUCKET_LICENSE=$(echo $BITBUCKET_LICENSE | sed ':a;N;$!ba;s/\n/ /g' | sed 's/ /\\u000a/g')

    mkdir -p $BITBUCKET_HOME/shared
    chown ${RUN_USER}:${RUN_GROUP} $BITBUCKET_HOME/shared

    cat > $UNATTENDED_CONFIG <<EOF
setup.displayName = Bitbucket
setup.baseUrl = $BITBUCKET_URL
setup.license = $BITBUCKET_LICENSE
setup.sysadmin.username = ${BITBUCKET_USER:-admin}
setup.sysadmin.password = ${BITBUCKET_PASSWORD}
setup.sysadmin.displayName = Administrator
setup.sysadmin.emailAddress = admin@example.com
EOF
    chown ${RUN_USER}:${RUN_GROUP} $UNATTENDED_CONFIG
fi

if [ "$1" = 'bitbucket-server' ]; then
    ./install-plugin.sh &
    if [ "$RUN_USER" = "root" ]; then
        exec $BITBUCKET_INSTALL_DIR/bin/start-webapp.sh -fg
    else
        exec gosu $RUN_USER $BITBUCKET_INSTALL_DIR/bin/start-webapp.sh -fg
    fi
else
    exec "$@"
fi
