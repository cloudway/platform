FROM mongo:3.2.6

RUN apt-get update && apt-get install -y --no-install-recommends supervisor git

ENV CLOUDWAY_ROOT /usr/local/cloudway
ENV CLOUDWAY_DOMAIN example.com

COPY cloudway-broker.tar.gz cloudway-broker.tar.gz
RUN tar -xf cloudway-broker.tar.gz --strip-components=1 -C / \
 && rm cloudway-broker.tar.gz

RUN git clone git://github.com/cloudway/plugins.git \
 && ( cd plugins && ./install.sh ) \
 && rm -rf plugins

COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

WORKDIR $CLOUDWAY_ROOT
EXPOSE 6616
