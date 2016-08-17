FROM bitbucket-server:deps

RUN set -x \
 && apt-get update \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        supervisor vim nodejs npm redis-server \
        postfix sasl2-bin libsasl2-modules rsyslog openssh-client \
 && rm -rf /var/lib/apt/lists/*

#----------------------------------------------------------------------------

# add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r mongodb && useradd -r -g mongodb mongodb

# pub   4096R/A15703C6 2016-01-11 [expires: 2018-01-10]
#       Key fingerprint = 0C49 F373 0359 A145 1858  5931 BC71 1F9B A157 03C6
# uid                  MongoDB 3.4 Release Signing Key <packaging@mongodb.com>
RUN apt-key adv --keyserver ha.pool.sks-keyservers.net --recv-keys 0C49F3730359A14518585931BC711F9BA15703C6

ENV MONGO_MAJOR 3.3
ENV MONGO_VERSION 3.3.10

RUN echo "deb http://repo.mongodb.org/apt/debian jessie/mongodb-org/$MONGO_MAJOR main" > /etc/apt/sources.list.d/mongodb-org.list

RUN set -x \
	&& apt-get update \
	&& apt-get install -y \
		mongodb-org-unstable=$MONGO_VERSION \
		mongodb-org-unstable-server=$MONGO_VERSION \
		mongodb-org-unstable-shell=$MONGO_VERSION \
		mongodb-org-unstable-mongos=$MONGO_VERSION \
		mongodb-org-unstable-tools=$MONGO_VERSION \
	&& rm -rf /var/lib/apt/lists/* \
	&& rm -rf /var/lib/mongodb \
	&& mv /etc/mongod.conf /etc/mongod.conf.orig

#----------------------------------------------------------------------------

ENV HIPACHE_ROOT /hipache
ENV NODE_ENV production

RUN git clone https://github.com/cloudway/hipache $HIPACHE_ROOT \
 && npm install -g $HIPACHE_ROOT --production \
 && mkdir -p /var/log/hipache \
 && sed -i 's/daemonize yes/daemonize no/' /etc/redis/redis.conf

#----------------------------------------------------------------------------

ENV CLOUDWAY_ROOT /usr/local/cloudway
ENV CLOUDWAY_DOMAIN example.com

COPY cloudway-broker.tar.gz cloudway-broker.tar.gz
RUN tar -xf cloudway-broker.tar.gz --strip-components=1 -C / \
 && rm cloudway-broker.tar.gz \
 && cp -f /usr/bin/scp /usr/local/cloudway/sandbox/usr/bin/scp

RUN git clone git://github.com/cloudway/plugins.git \
 && ( cd plugins && ./install.sh ) \
 && rm -rf plugins

COPY supervisord.conf /etc/supervisor/conf.d/
COPY sbin/ ${CLOUDWAY_ROOT}/sbin/
COPY repo-deployer.jar $BITBUCKET_INSTALL_DIR/

VOLUME /data
WORKDIR $CLOUDWAY_ROOT
EXPOSE 80 7999 2200

ENTRYPOINT ["/usr/local/cloudway/sbin/entrypoint.sh"]
