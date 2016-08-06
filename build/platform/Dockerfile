FROM java:openjdk-8-jre

# grab gosu for easy step-down from root
ENV GOSU_VERSION 1.7
RUN set -x \
    && apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        ca-certificates wget curl git libtcnative-1 supervisor vim \
        postfix sasl2-bin libsasl2-modules rsyslog openssh-client \
        nodejs npm redis-server \
    && rm -rf /var/lib/apt/lists/* \
    && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$(dpkg --print-architecture)" \
    && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$(dpkg --print-architecture).asc" \
    && export GNUPGHOME="$(mktemp -d)" \
    && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
    && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
    && rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc \
    && chmod +x /usr/local/bin/gosu \
    && gosu nobody true

# The default installation recommends an unprivileged account. We use the root
# user to allow bitbucket to access docker socket.
ENV RUN_USER=root RUN_GROUP=root

# https://confluence.atlassian.com/display/BitbucketServer/Bitbucket+Server+home+directory
ENV BITBUCKET_HOME /data/bitbucket

# Install Atlassian Bitbucket Server to the following location
ENV BITBUCKET_INSTALL_DIR /opt/atlassian/bitbucket

ENV BITBUCKET_VERSION 4.6.3
ENV BITBUCKET_TGZ_URL https://downloads.atlassian.com/software/stash/downloads/atlassian-bitbucket-${BITBUCKET_VERSION}.tar.gz
RUN mkdir -p ${BITBUCKET_INSTALL_DIR} \
    && curl -L ${BITBUCKET_TGZ_URL} | tar -xz --strip=1 -C "$BITBUCKET_INSTALL_DIR" \
    && mkdir -p ${BITBUCKET_INSTALL_DIR}/conf/Catalina \
    && chmod -R 700 ${BITBUCKET_INSTALL_DIR}/conf/Catalina \
    && chmod -R 700 ${BITBUCKET_INSTALL_DIR}/logs \
    && chmod -R 700 ${BITBUCKET_INSTALL_DIR}/temp \
    && chmod -R 700 ${BITBUCKET_INSTALL_DIR}/work \
    && chown -R ${RUN_USER}:${RUN_GROUP} ${BITBUCKET_INSTALL_DIR}/              \
    && ln --symbolic "/usr/lib/x86_64-linux-gnu/libtcnative-1.so" "${BITBUCKET_INSTALL_DIR}/lib/native/libtcnative-1.so"

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
