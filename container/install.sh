#!/bin/bash -e

BASE_DIR=${BASE_DIR-$PWD}

if [ "$INSTALL_DIR" == "/" ]; then
  INSTALL_DIR=""
fi

if [ -z "$INSTALL_DIR" -a $(id -u) -ne 0 ]; then
  BASE_DIR="$BASE_DIR" INSTALL_DIR="$INSTALL_DIR" exec sudo $0
fi

CLOUDWAY_HOME=${INSTALL_DIR}/usr/share/cloudway
CLOUDWAY_DATA=${INSTALL_DIR}/var/lib/cloudway

# Install SELinux policy module
pushd ${BASE_DIR}/misc/selinux > /dev/null
make -f /usr/share/selinux/devel/Makefile load clean
popd > /dev/null

/usr/sbin/setsebool -P httpd_unified=on httpd_can_network_connect=on httpd_can_network_relay=on \
                       httpd_read_user_content=on httpd_enable_homedirs=on httpd_execmem=on \
                       allow_polyinstantiation=on || :

# Install jar files
mkdir -p ${CLOUDWAY_HOME}/lib
cp ${BASE_DIR}/target/*.jar ${CLOUDWAY_HOME}/lib/
cp ${BASE_DIR}/target/dependencies/*.jar ${CLOUDWAY_HOME}/lib/

# Install executable files
mkdir -p ${CLOUDWAY_HOME}/bin
cp ${BASE_DIR}/misc/bin/* ${CLOUDWAY_HOME}/bin/
chmod 0755 ${CLOUDWAY_HOME}/bin/*
mkdir -p ${INSTALL_DIR}/usr/bin
ln -sf ${CLOUDWAY_HOME}/bin/* ${INSTALL_DIR}/usr/bin/

mkdir -p ${CLOUDWAY_HOME}/libexec
cp ${BASE_DIR}/misc/libexec/* ${CLOUDWAY_HOME}/libexec/
chmod 0755 ${CLOUDWAY_HOME}/libexec/*

/sbin/restorecon /usr/share/cloudway/bin/cwctl || :
/sbin/restorecon /usr/share/cloudway/libexec/cwunidle.sh || :
/sbin/restorecon /usr/share/cloudway/libexec/cwautoidle.py || :

# Install configuration files
mkdir -p ${INSTALL_DIR}/etc/cloudway
cp -r ${BASE_DIR}/misc/conf/* ${INSTALL_DIR}/etc/cloudway/
ln -snf ${INSTALL_DIR}/etc/cloudway ${CLOUDWAY_HOME}/conf

# Install system configuration files
mkdir -p ${INSTALL_DIR}/etc
cp -r ${BASE_DIR}/misc/etc/* ${INSTALL_DIR}/etc/
mkdir -p ${INSTALL_DIR}/var/www
cp -r ${BASE_DIR}/misc/www/* ${INSTALL_DIR}/var/www/

if [ -z "$INSTALL_DIR" ]; then
  # Configure system services
  if [ -x /usr/bin/systemctl ]; then
    systemctl enable cwadminctl.service || :
    systemctl enable oddjobd.service || :
    systemctl enable cgred.service || :
    systemctl enable cgconfig.service || :
    systemctl restart oddjobd.service || :
    systemctl restart cgred.service || :
  else
    chkconfig --add cwadminctl || :
    chkconfig --add oddjobd || :
    chkconfig --add cgred || :
    chkconfig --add cgconfig || :
    service oddjobd restart || :
    service cgred restart || :
  fi

  # Configure apache
  function apachedb {
    local dbf=${CLOUDWAY_DATA}/.httpd/$1
    if [ ! -f $dbf.db ]; then
      touch $dbf.txt
      httxt2dbm -f DB -i $dbf.txt -o $dbf.db
    fi
  }

  mkdir -p ${CLOUDWAY_DATA}/.httpd
  apachedb mappings
  apachedb aliases
  apachedb idles

  apachectl restart || :
  iptables -I INPUT -p tcp --dport 80 -j ACCEPT
fi
