FROM hipache:0.3.1

# patch the hipache package
RUN sed -i 's/"http-proxy": "1.0.2"/"http-proxy": "1.11.1"/' /hipache/package.json && \
    npm install -g /hipache --production && \
    mkdir -p /var/log/nginx

# make changes to redis config
RUN sed -i 's/daemonize yes/daemonize no/' /etc/redis/redis.conf

# add proxy updater service
COPY cwman add-mapping /usr/bin/
COPY cloudway.conf /usr/local/cloudway/conf/cloudway.conf
COPY config.json /hipache/config/config.json
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf

ENTRYPOINT ["supervisord", "-c", "/etc/supervisor/supervisord.conf"]
