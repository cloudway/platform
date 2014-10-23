# Remove symbolic links
function rmln {
  for f in $1/*; do
    [ -h "$2/$(basename $f)" ] && rm "$2/$(basename $f)"
  done
}

rmln /usr/share/cloudway/bin /usr/bin
rmln /usr/share/cloudway/sbin /usr/sbin
rmln /usr/share/cloudway/addons /var/lib/cloudway/.addons

# Remove system services
if [ $1 -eq 0 ]; then
%if 0%{?fedora} >= 16 || 0%{?rhel} >= 7
  systemctl disable cloudway-worker.service
%else
  chkconfig --del cloudway-worker
%endif
fi
