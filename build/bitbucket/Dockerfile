FROM java:openjdk-8-jre

# grab gosu for easy step-down from root
ENV GOSU_VERSION 1.7
RUN set -x \
    && apt-get update && apt-get install -y --no-install-recommends ca-certificates wget && rm -rf /var/lib/apt/lists/* \
    && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$(dpkg --print-architecture)" \
    && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$(dpkg --print-architecture).asc" \
    && export GNUPGHOME="$(mktemp -d)" \
    && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
    && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
    && rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc \
    && chmod +x /usr/local/bin/gosu \
    && gosu nobody true

# Install git, download and extract Bitbucket Server and create the required directory layout.
# Try to limit the number of RUN instructions to minimise the number of layers that will need to be created.
RUN apt-get update -qq \
    && apt-get install -y --no-install-recommends git libtcnative-1 \
    && apt-get clean autoclean \
    && apt-get autoremove --yes \
    && rm -rf /var/lib/{apt,dpkg,cache,log}/

# The default installation recommends an unprivileged account. We use the root
# user to allow bitbucket to access docker socket.
ENV RUN_USER=root RUN_GROUP=root

# https://confluence.atlassian.com/display/BitbucketServer/Bitbucket+Server+home+directory
ENV BITBUCKET_HOME /var/atlassian/application-data/bitbucket

# Install Atlassian Bitbucket Server to the following location
ENV BITBUCKET_INSTALL_DIR /opt/atlassian/bitbucket
WORKDIR $BITBUCKET_INSTALL_DIR

ENV BITBUCKET_VERSION 4.6.3
ENV DOWNLOAD_URL https://downloads.atlassian.com/software/stash/downloads/atlassian-bitbucket-${BITBUCKET_VERSION}.tar.gz
RUN mkdir -p ${BITBUCKET_INSTALL_DIR} \
    && curl -L ${DOWNLOAD_URL} | tar -xz --strip=1 -C "$BITBUCKET_INSTALL_DIR" \
    && mkdir -p ${BITBUCKET_INSTALL_DIR}/conf/Catalina \
    && chmod -R 700 ${BITBUCKET_INSTALL_DIR}/conf/Catalina \
    && chmod -R 700 ${BITBUCKET_INSTALL_DIR}/logs \
    && chmod -R 700 ${BITBUCKET_INSTALL_DIR}/temp \
    && chmod -R 700 ${BITBUCKET_INSTALL_DIR}/work \
    && chown -R ${RUN_USER}:${RUN_GROUP} ${BITBUCKET_INSTALL_DIR}/              \
    && ln --symbolic "/usr/lib/x86_64-linux-gnu/libtcnative-1.so" "${BITBUCKET_INSTALL_DIR}/lib/native/libtcnative-1.so"

# install cloudway repository deployer
COPY cwman /usr/bin/cwman
COPY repo-deployer-*.jar repo-deployer.jar

# HTTP and SSH port
EXPOSE 7990 7999

COPY entrypoint.sh install-plugin.sh ./
ENTRYPOINT ["./entrypoint.sh"]
CMD ["bitbucket-server"]
