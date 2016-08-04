FROM icloudway/hipache:0.4.0

# add proxy updater service
COPY cwman add-mapping /usr/bin/
COPY cloudway.conf /usr/local/cloudway/conf/cloudway.conf
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf

ENTRYPOINT ["supervisord", "-c", "/etc/supervisor/supervisord.conf"]
