#!/bin/bash

: ${CLOUDWAY_DOMAIN:=example.com}
export CLOUDWAY_DOMAIN

chmod 0640 /usr/local/cloudway/conf/cloudway.conf
if [ "$CLOUDWAY_DOMAIN" != 'example.com' ]; then
    sed -i "s/example\\.com/$CLOUDWAY_DOMAIN/g" /usr/local/cloudway/conf/cloudway.conf
fi

# configure bitbucket
if [ -z "$CLOUDWAY_SCM_URL" -a -n "$BITBUCKET_ENV_BITBUCKET_URL" ]; then
    BITBUCKET_HOST=$BITBUCKET_PORT_7990_TCP_ADDR
    BITBUCKET_PORT=$BITBUCKET_PORT_7990_TCP_PORT
    BITBUCKET_USER=${BITBUCKET_ENV_BITBUCKET_USER:-admin}
    BITBUCKET_PASS=$BITBUCKET_ENV_BITBUCKET_PASSWORD
    BITBUCKET_URL="http://${BITBUCKET_USER}:${BITBUCKET_PASS}@${BITBUCKET_HOST}:${BITBUCKET_PORT}"

    cwman config "scm.type" "bitbucket"
    cwman config "scm.url" "$BITBUCKET_URL"
    cwman config "scm.clone_url" "git clone ssh://git@git.${CLOUDWAY_DOMAIN}:7999/<namespace>/<repo>.git"
fi

# configure postfix
if [ ! -x /usr/local/bin/postfix.sh ]; then
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

    cat > /usr/local/bin/postfix.sh <<EOF
#!/bin/bash -e
trap "/usr/sbin/postfix stop" EXIT
trap "/usr/sbin/postfix reload" SIGHUP
/usr/sbin/postfix start
sleep infinity
EOF
    chmod +x /usr/local/bin/postfix.sh
fi

exec supervisord -c /etc/supervisor/supervisord.conf
