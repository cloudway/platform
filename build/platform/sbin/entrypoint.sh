#!/bin/bash
set -e

: ${CLOUDWAY_DOMAIN:=example.com}
: ${CONSOLE_URL:=http://api.${CLOUDWAY_DOMAIN}}
: ${GIT_URL:=http://git.${CLOUDWAY_DOMAIN}}

export CLOUDWAY_DOMAIN

get_domain() {
    domain=${1#*//}
    domain=${domain%:*}
    echo $domain
}

CONSOLE_DOMAIN=$(get_domain $CONSOLE_URL)
GIT_DOMAIN=$(get_domain $GIT_URL)

cwman config "domain" ${CLOUDWAY_DOMAIN}
cwman config "console.url" ${CONSOLE_URL}
cwman config "proxy.url" hipache://127.0.0.1:6379
cwman config "proxy-mapping.${CONSOLE_DOMAIN}" http://127.0.0.1:6616
cwman config "scm.type" mock
cwman config "scm.url" "file:///data/git"
cwman config "scm.clone_url" "git clone ssh://git@${GIT_DOMAIN}:7999/<namespace>/<repo>.git"

# Install plugins
cwman config "hub.dir" /data/plugins
for d in $CLOUDWAY_ROOT/plugins/*; do
    [ -f $d/manifest/plugin.yml ] && cwman install $d
done

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
