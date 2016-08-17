Cloudway: <small>The way to cloud</small>
==========================================

[![Release](https://img.shields.io/github/release/cloudway/platform.svg)](https://github.com/cloudway/platform/releases/latest)
[![Build Status](https://travis-ci.org/cloudway/platform.svg?branch=develop)](https://travis-ci.org/cloudway/platform)
[![Docker Pulls](https://img.shields.io/docker/pulls/icloudway/platform.svg)](https://hub.docker.com/r/icloudway/platform/)

## 简介

云途（Cloudway）是一个基于 [docker](https://docker.com) 的平台即服务(PaaS)架构,
是贯穿于开发与运维各个阶段的统一平台。云途屏蔽了系统架构的底层细节, 使开发者专注于业务本身。
云途提供了以下措施帮助应用开发者快速开发与运行应用系统:

- 多种应用开发框架

  提供多种流行的应用开发框架, 包括 PHP, Java, Python, Ruby, Node.JS 等等。

- 多种系统服务

  提供多种 SQL 与 NoSQL 数据库、消息队列、缓存等系统服务。

- 可扩充

  应用开发框架与系统服务均可定制, 你可以编写自己的框架或服务插件。

- 微服务架构

  对微服务架构提供平台级支持。通过合理设计应用架构, 将一个单体应用分解成多个服务,
  云途平台负责将它们拼装起来, 并提供自动服务发现、容灾、负载均衡等措施, 使应用
  开发者不必为服务之间的互通耗费过多精力。

- 快速应用开发与部署

  云途使用`git`管理应用代码, 可以与你的开发流程结合起来, 执行`git push`
  即可快速部署应用, 并且可以随时切换应用分支, 这样在发现应用错误时不会产生过长的停顿。
  云途平台使得开发与运维不再泾渭分明。

- 管理与监控

  云途提供了多种工具帮助管理与监控应用系统。

## 安装与运行

1. 安装 docker, 请参考 https://docs.docker.com/engine/installation/

2. 拖拽云途最新镜像

        $ docker pull icloudway/platform:latest

3. 运行以下脚本启动云途

        #!/bin/bash -e

        DOMAIN=example.com
        CONSOLE_URL=http://api.$DOMAIN
        GIT_URL=http://git.$DOMAIN

        : ${CLOUDWAY_TAG:=latest}

        cd "$(dirname "$BASH_SOURCE")"

        # generate random password
        if [ ! -e scm-password ]; then
            LC_CTYPE=C tr -cd '[:alnum:]' < /dev/urandom | fold -w20 | head -n1 > scm-password
        fi

        # start all-in-one container
        docker run -d --name cloudway-platform --restart=always \
                   -e CLOUDWAY_DOMAIN=$DOMAIN \
                   -e CONSOLE_URL=$CONSOLE_URL \
                   -e BITBUCKET_URL=$GIT_URL \
                   -e BITBUCKET_PASSWORD="$(< scm-password)" \
                   -e BITBUCKET_LICENSE="$(< scm-license)" \
                   -v /var/run/docker.sock:/var/run/docker.sock:ro \
                   -v cloudway-data:/data \
                   -p 80:80 -p 7999:7999 -p 2200:2200 \
                   icloudway/platform:${CLOUDWAY_TAG}

4. 设置DNS服务器, 推荐使用`dnsmasq`, 修改`/etc/dnsmasq.conf`, 添加以下配置并重启`dnsmasq`:

        address=/example.com/192.168.99.100

   其中`example.com`是在运行脚本中设置的根域名, `192.168.99.100`是docker主机的IP地址,
   该配置将设置一个通配域名`*.example.com`。

   在客户机中修改DNS设置, 将DNS服务器地址指向`dnsmasq`所在主机的IP地址。

5. 使用浏览器打开`http://api.example.com`, 点击注册创建新的帐号。以新建帐号登录,
按提示设置名字空间并上传SSH公共密钥, 开始创建应用。
