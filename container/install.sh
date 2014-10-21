#!/bin/bash -e

BASE_DIR=${BASE_DIR-$PWD}

if [ "$INSTALL_DIR" == "/" ]; then
  INSTALL_DIR=""
fi

if [ -z "$INSTALL_DIR" -a $(id -u) -ne 0 ]; then
  BASE_DIR="$BASE_DIR" INSTALL_DIR="$INSTALL_DIR" exec sudo $0
fi

CLOUDWAY_HOME=${INSTALL_DIR}/opt/cloudway
CLOUDWAY_CONF=${INSTALL_DIR}/etc/cloudway
CLOUDWAY_DATA=${INSTALL_DIR}/var/lib/cloudway

# Install jar files
mkdir -p ${CLOUDWAY_HOME}/lib
cp ${BASE_DIR}/target/*.jar ${CLOUDWAY_HOME}/lib/
cp ${BASE_DIR}/target/dependencies/*.jar ${CLOUDWAY_HOME}/lib/

# Install executable files
mkdir -p ${CLOUDWAY_HOME}/{bin,sbin}
cp ${BASE_DIR}/misc/bin/* ${CLOUDWAY_HOME}/bin/
cp ${BASE_DIR}/misc/sbin/* ${CLOUDWAY_HOME}/sbin/
chmod 0755 ${CLOUDWAY_HOME}/bin/*
chmod 0750 ${CLOUDWAY_HOME}/sbin/*
mkdir -p ${INSTALL_DIR}/usr/{bin,sbin}
ln -sf ${CLOUDWAY_HOME}/bin/* ${INSTALL_DIR}/usr/bin/
ln -sf ${CLOUDWAY_HOME}/sbin/* ${INSTALL_DIR}/usr/sbin/

# Install configuration files
mkdir -p ${CLOUDWAY_CONF}
cp -r ${BASE_DIR}/misc/conf/* ${CLOUDWAY_CONF}/

# Install addons
mkdir -p ${CLOUDWAY_HOME}/libexec/addons
cp -rp ${BASE_DIR}/../addons/* ${CLOUDWAY_HOME}/libexec/addons/
chmod 0755 ${CLOUDWAY_HOME}/libexec/addons/*/bin/*
ln -sf ${CLOUDWAY_HOME}/libexec/addons ${CLOUDWAY_DATA}/.addons

if [ -z "$INSTALL_DIR" ]; then
  # Configure oddjob 
  mkdir -p ${INSTALL_DIR}/etc/oddjobd.conf.d
  cp ${BASE_DIR}/misc/etc/oddjob/oddjobd-cloudway.conf ${INSTALL_DIR}/etc/oddjobd.conf.d/
  mkdir -p ${INSTALL_DIR}/etc/dbus-1/system.d
  cp ${BASE_DIR}/misc/etc/oddjob/com.cloudway.oddjob.conf ${INSTALL_DIR}/etc/dbus-1/system.d/

  if [ -x /usr/bin/systemctl ]; then
    systemctl restart oddjobd.service || :
  else
    service oddjobd restart || :
  fi

  # Configure system services
  if [ -x /usr/bin/systemctl ]; then
    mkdir -p ${INSTALL_DIR}/etc/systemd/system
    cp ${BASE_DIR}/misc/etc/services/* ${INSTALL_DIR}/etc/systemd/system/
    systemctl enable cloudway-worker.service
  else
    mkdir -p ${INSTALL_DIR}/etc/rc.d/init.d
    cp ${BASE_DIR}/misc/etc/init.d/* ${INSTALL_DIR}/etc/rc.d/init.d/
    /sbin/chkconfig --add cloudway-worker
  fi

  # Configure apache
  mkdir -p ${INSTALL_DIR}/etc/httpd
  cp -r ${BASE_DIR}/misc/etc/httpd/* ${INSTALL_DIR}/etc/httpd/
  mkdir -p ${INSTALL_DIR}/var/www/html
  cp -r ${BASE_DIR}/misc/www/html/* ${INSTALL_DIR}/var/www/html/

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
