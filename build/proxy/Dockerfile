FROM debian:jessie

RUN apt-get update && \
    apt-get install -y --no-install-recommends nodejs npm git supervisor redis-server && \
    rm -rf /var/lib/apt/files/*

ENV HIPACHE_ROOT /hipache
ENV NODE_ENV production

# install hipache
RUN git clone https://github.com/cloudway/hipache $HIPACHE_ROOT \
 && npm install -g $HIPACHE_ROOT --production \
 && mkdir -p /var/log/hipache \
 && sed -i 's/daemonize yes/daemonize no/' /etc/redis/redis.conf

# add proxy updater service
COPY cwman add-mapping /usr/bin/
COPY cloudway.conf /usr/local/cloudway/conf/cloudway.conf
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf

EXPOSE 80
ENTRYPOINT ["supervisord", "-c", "/etc/supervisor/supervisord.conf"]
