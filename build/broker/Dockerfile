FROM mongo:3.2.6

RUN apt-get update && apt-get install -y --no-install-recommends supervisor git

ENV CLOUDWAY_ROOT=/usr/local/cloudway \
    CLOUDWAY_DOMAIN=example.com \
    VIRTUAL_HOST=api.example.com \
    VIRTUAL_PORT=6616

COPY files/ $CLOUDWAY_ROOT/
COPY cwman /usr/bin
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf

RUN git clone git://github.com/cloudway/plugins.git \
 && ( cd plugins && ./install.sh ) \
 && rm -rf plugins

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
