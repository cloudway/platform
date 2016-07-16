#!/bin/bash

curl -H "Content-Type: application/json" --data '{"build": true}' -X POST https://registry.hub.docker.com/u/icloudway/bitbucket-server/trigger/82acd6d7-5ceb-4424-a4d3-e78ec577ef08/
curl -H "Content-Type: application/json" --data '{"build": true}' -X POST https://registry.hub.docker.com/u/icloudway/broker/trigger/472ec578-2355-468d-99f8-6bba44496984/
curl -H "Content-Type: application/json" --data '{"build": true}' -X POST https://registry.hub.docker.com/u/icloudway/proxy/trigger/5210e0c5-3da8-40ce-8fff-ceef585d3cd6/
curl -H "Content-Type: application/json" --data '{"build": true}' -X POST https://registry.hub.docker.com/u/icloudway/sshd/trigger/96ec948d-16e4-4814-88fd-e1e2a3368954/
