#!/bin/bash

if [ "$CLOUDWAY_DOMAIN" != 'example.com' ]; then
    sed -i "s/example\\.com/$CLOUDWAY_DOMAIN/g" /usr/local/cloudway/conf/cloudway.conf
fi

exec supervisord -c /etc/supervisor/supervisord.conf
