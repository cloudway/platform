#!/bin/bash
set -e

: ${CLOUDWAY_DOMAIN:=example.com}
: ${CONSOLE_URL:=http://api.${CLOUDWAY_DOMAIN}}
: ${BITBUCKET_URL:=http://git.${CLOUDWAY_DOMAIN}}

export CLOUDWAY_DOMAIN

get_domain() {
    domain=${1#*//}
    domain=${domain%:*}
    echo $domain
}

CONSOLE_DOMAIN=$(get_domain $CONSOLE_URL)
BITBUCKET_DOMAIN=$(get_domain $BITBUCKET_URL)

cwman config "domain" ${CLOUDWAY_DOMAIN}
cwman config "console.url" ${CONSOLE_URL}
cwman config "proxy.url" hipache://127.0.0.1:6379
cwman config "proxy-mapping.${CONSOLE_DOMAIN}" http://127.0.0.1:6616
cwman config "proxy-mapping.${BITBUCKET_DOMAIN}" http://127.0.0.1:7990
cwman config "scm.type" bitbucket
cwman config "scm.url" "http://${BITBUCKET_USER:-admin}:${BITBUCKET_PASSWORD?}@127.0.0.1:7990"
cwman config "scm.clone_url" "git clone ssh://git@${BITBUCKET_DOMAIN}:7999/<namespace>/<repo>.git"

# allow the container to be started with `--user`
mkdir -p $BITBUCKET_HOME
chown ${RUN_USER}:${RUN_GROUP} $BITBUCKET_HOME

# https://jira.atlassian.com/browse/BSERV-8345
sed -i $BITBUCKET_INSTALL_DIR/bin/setenv.sh \
    -e 's|JVM_SUPPORT_RECOMMENDED_ARGS=""|JVM_SUPPORT_RECOMMENDED_ARGS="-Djava.security.egd=file:/dev/./urandom"|'

UNATTENDED_CONFIG=$BITBUCKET_HOME/shared/bitbucket.properties
if [ ! -e $UNATTENDED_CONFIG ]; then
    if [ -z "$BITBUCKET_PASSWORD" -o -z "$BITBUCKET_LICENSE" ]; then
        echo >&2 'You must specifiy the BITBUCKET_PASSWORD and BITBUCKET_LICENSE environment variables'
        exit 1
    fi

    # Use the \u000a character to not break the license over multiple lines
    BITBUCKET_LICENSE=$(echo $BITBUCKET_LICENSE | sed ':a;N;$!ba;s/\n/ /g' | sed 's/ /\\u000a/g')

    mkdir -p $BITBUCKET_HOME/shared
    chown ${RUN_USER}:${RUN_GROUP} $BITBUCKET_HOME/shared

    cat > $UNATTENDED_CONFIG <<EOF
setup.displayName = Bitbucket
setup.baseUrl = ${BITBUCKET_URL}
setup.license = ${BITBUCKET_LICENSE}
setup.sysadmin.username = ${BITBUCKET_USER:-admin}
setup.sysadmin.password = ${BITBUCKET_PASSWORD}
setup.sysadmin.displayName = Administrator
setup.sysadmin.emailAddress = admin@example.com
EOF
    chown ${RUN_USER}:${RUN_GROUP} $UNATTENDED_CONFIG
fi

# Initialize mongodb data directory
mkdir -p /data/db /data/configdb
chown -R mongodb /data/db /data/configdb

cwman config userdb.url mongodb://127.0.0.1:27017/cloudway

# configure postfix
if [ ! -x $CLOUDWAY_ROOT/sbin/postfix.sh ]; then
    postconf -e myhostname=$CLOUDWAY_DOMAIN
    postconf -e mydestination=$CLOUDWAY_DOMAIN,localhost
    postconf -e smtpd_sasl_auth_enable=yes
    postconf -e broken_sasl_auth_clients=yes
    postconf -e smtpd_recipient_restrictions=permit_sasl_authenticated,reject_unauth_destination
    postconf -e smtpd_use_tls=no
    postconf -e inet_interfaces=127.0.0.1
    postconf -F '*/*/chroot = n'

    cat >> /etc/postfix/sasl/smtpd.conf <<EOF
pwcheck_method: auxprop
auxprop_plugin: sasldb
mech_list: PLAIN LOGIN CRAM-MD5 DIGEST-MD5 NTLM
EOF

    # generate random password
    username=postmaster
    password=$(tr -cd '[:alnum:]' < /dev/urandom | fold -w20 | head -n1)
    echo $password | saslpasswd2 -p -c -u $CLOUDWAY_DOMAIN $username
    chown postfix.sasl /etc/sasldb2

    cwman config "smtp.host"     "127.0.0.1"
    cwman config "smtp.port"     "25"
    cwman config "smtp.username" "$username"
    cwman config "smtp.password" "$password"
    cwman config "smtp.from"     "Cloudway <${username}@${CLOUDWAY_DOMAIN}>"

    cat > $CLOUDWAY_ROOT/sbin/postfix.sh <<EOF
#!/bin/bash -e
trap "/usr/sbin/postfix stop" EXIT
trap "/usr/sbin/postfix reload" SIGHUP
/usr/sbin/postfix start
sleep infinity
EOF
    chmod +x $CLOUDWAY_ROOT/sbin/postfix.sh
fi

exec supervisord
