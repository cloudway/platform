prefix=${RPM_INSTALL_PREFIX}
if [ "$prefix" == "/" ]; then
  prefix=""
fi

homedir=${prefix}/usr/share/cloudway
datadir=${prefix}/var/lib/cloudway

mkdir -p ${prefix}/usr/bin
ln -sf ${homedir}/bin/* ${prefix}/usr/bin/
ln -snf /etc/cloudway ${homedir}/conf

/usr/sbin/semodule -i /var/lib/selinux/packages/cloudway.pp.bz2 || :
/usr/sbin/setsebool -P httpd_unified=on httpd_can_network_connect=on httpd_can_network_relay=on \
                       httpd_read_user_content=on httpd_enable_homedirs=on httpd_execmem=on \
                       allow_polyinstantiation=on || :

/sbin/restorecon /usr/share/cloudway/bin/cwctl || :
/sbin/restorecon /usr/share/cloudway/libexec/cwunidle.sh || :
/sbin/restorecon /usr/share/cloudway/libexec/cwautoidle.py || :

if [ -z "$prefix" ]; then
%if 0%{?fedora} >= 16 || 0%{?rhel} >= 7
  systemctl enable cwadminctl.service || :
  systemctl enable oddjobd.service || :
  systemctl enable cgred.service || :
  systemctl restart oddjobd.service || :
  systemctl restart cgred.service || :
%else
  chkconfig --add cwadminctl || :
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
  chown root:apache ${datadir}/.httpd
  chmod 0750 ${datadir}/.httpd

  touch ${datadir}/.httpd/containers.txt
  apachedb mappings
  apachedb aliases
  apachedb idles

  apachectl restart || :
  iptables -I INPUT -p tcp --dport 80 -j ACCEPT || :
fi