# Remove symbolic links
for f in /usr/share/cloudway/bin/*; do
  [ -h "/usr/bin/$(basename $f)" ] && rm "/usr/bin/$(basename $f)"
done

# Remove system services
if [ $1 -eq 0 ]; then
%if 0%{?fedora} >= 16 || 0%{?rhel} >= 7
  systemctl disable cwadminctl.service
%else
  chkconfig --del cwadminctl
%endif
fi
