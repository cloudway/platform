FROM hipache:0.3.1

# patch the package
RUN sed -i 's/"http-proxy": "1.0.2"/"http-proxy": "1.11.1"/' /hipache/package.json && \
    npm install -g /hipache --production && \
    mkdir -p /var/log/nginx

# add proxy updater service
ADD cwman /usr/bin
ADD config.json /hipache/config/config.json
ADD supervisord.conf /etc/supervisor/conf.d/supervisord.conf
ENV CLOUDWAY_PROXY_HOST hipache://127.0.0.1:6379
