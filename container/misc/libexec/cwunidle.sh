#!/bin/bash
exec /usr/bin/oddjob_request \
    -s com.cloudway.oddjob \
    -o /com/cloudway/oddjob \
    -i com.cloudway.oddjob.unidler \
    unidle $1
