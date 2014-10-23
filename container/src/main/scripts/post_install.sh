prefix=${RPM_INSTALL_PREFIX}
if [ "$prefix" == "/" ]; then
  prefix=""
fi

homedir=${prefix}/usr/share/cloudway
datadir=${prefix}/var/lib/cloudway

mkdir -p ${prefix}/usr/{bin,sbin}
ln -sf ${homedir}/bin/* ${prefix}/usr/bin/
ln -sf ${homedir}/sbin/* ${prefix}/usr/sbin/

mkdir -p ${homedir}/conf
ln -sf /etc/cloudway/* ${homedir}/conf/

chmod 0755 ${homedir}/addons/*/bin/*
mkdir -p ${datadir}/.addons
ln -sf ${homedir}/addons/* ${datadir}/.addons/

if [ -z "$prefix" ]; then
%if 0%{?fedora} >= 16 || 0%{?rhel} >= 7
  systemctl enable cloudway-worker.service || :
  systemctl enable oddjobd.service || :
  systemctl enable cgred.service || :
  systemctl restart oddjobd.service || :
  systemctl restart cgred.service || :
%else
  chkconfig --add cloudway-worker || :
  chkconfig --add oddjobd || :
  chkconfig --add cgred || :
  service oddjobd restart || :
  service cgred restart || :
%endif

  function apachedb {
    local dbf=${datadir}/.httpd/$1
    if [ ! -f $dbf.db ]; then
      touch $dbf.txt
      httxt2dbm -f DB -i $dbf.txt -o $dbf.db
    fi
  }

  mkdir -p ${datadir}/.httpd
  apachedb mappings
  apachedb aliases
  apachedb idles

  apachectl restart || :
  iptables -I INPUT -p tcp --dport 80 -j ACCEPT || :
fi