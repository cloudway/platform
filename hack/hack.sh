#!/bin/bash -e

cd "$(dirname "$BASH_SOURCE")/.."
version=$(< ./VERSION)

# copy changed files to containers
docker cp bundles/$version/binary-server/cwman-$version cloudway-broker:/usr/bin/cwman
docker cp bundles/$version/binary-sandbox/cwctl-$version cloudway-broker:/usr/local/cloudway/sandbox/usr/bin/cwctl
docker cp build/broker/files/views cloudway-broker:/usr/local/cloudway/
docker cp bundles/$version/binary-server/cwman-$version cloudway-bitbucket:/usr/bin/cwman
docker cp bundles/$version/binary-server/cwman-$version cloudway-proxy:/usr/bin/cwman

# restart containers
docker restart cloudway-broker cloudway-proxy

# show logs
docker exec -t cloudway-broker tail -f /var/log/supervisor/apiserver.log
