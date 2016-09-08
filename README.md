Cloudway: <small>The way to cloud</small>
==========================================

[![Release](https://img.shields.io/github/release/cloudway/platform.svg)](https://github.com/cloudway/platform/releases/latest)
[![Build Status](https://travis-ci.org/cloudway/platform.svg?branch=develop)](https://travis-ci.org/cloudway/platform)
[![Docker Pulls](https://img.shields.io/docker/pulls/icloudway/platform.svg)](https://hub.docker.com/r/icloudway/platform/)

## 简介

云途是一个基于 [docker](https://www.docker.com) 的平台即服务（PaaS）架构，是贯穿与开发与运维各个阶段的统一平台。云途屏蔽了系统架构的底层细节，使开发者专注于业务本身。

作为一名现代应用开发者需要了解很多的知识，不仅要熟练掌握编程语言，还需要了解底层的软硬件环境、网络架构、数据库管理、中间件配置等等。应用系统可能要求在一个广泛的环境下运行，从一个简单的单机服务器，到一个内部局域网络集群，进而到一个大规模的云计算环境。不同的环境需要不同的运维人员，并且与开发人员必须建立一个良好的沟通。因此，[DevOps](https://zh.wikipedia.org/wiki/DevOps) 越来越受到人们的重视，**DevOps** 是一种重视“软件开发人员（Dev）”和“IT运维人员（Ops）”之间沟通合作的文化、运动或惯例。透过自动化“软件交付”和“架构变更”的流程，来使得构建、测试、发布软件能够更加地快捷、频繁和可靠（来自维基百科）。

云途试图以统一的工具和方法来处理软件开发与软件交付过程，使两者之间不会出现鸿沟。开发团队可以使用敏捷或其它软件开发方法，当实现一个新特性或修正一个错误时，云途立即开始一个自动化的流程，自动完成软件的构建、测试与部署。我们将这种工作方式称之为“提交即交付”。虽然这并不是全新的概念，但由于云途本身就是一个软件构建工具，因此你可以不再需要第三方工具的支持。

云途同时也是一个完善的软件运行环境，它具有集群、应用伸缩、自动服务发现等诸多特性，能够满足大多数应用系统对运行环境的要求。云途是一个多租户 PaaS 平台，能够同时运行多个应用系统，每个应用运行在独立的容器中，互相之间的资源被隔离，具有可靠的安全保障。

应用系统可以采用[微服务](https://zh.wikipedia.org/wiki/微服務)架构将一个复杂的单体应用分解成多个服务。**微服务**利用模块化的方式组合出复杂的大型应用系统，各模块之间使用与语言无关的 API 相互通讯。云途通过自动服务发现将这些服务拼装起来，并根据运行负载自动伸缩或进行失效迁移。


## 安装与运行

1. 安装 docker

    请参考 https://docs.docker.com/engine/installation/

2. 拖取云途最新镜像

    ```shell
    $ docker pull icloudway/platform:latest
    ```

3. 运行以下脚本启动云途

    ```shell
    #!/bin/bash -e

    : ${CLOUDWAY_DOMAIN:=example.com}
    : ${CONSOLE_URL:=http://api.$DOMAIN}
    : ${CLOUDWAY_TAG:=latest}

    # start all-in-one container
    docker run -d --name cloudway-platform --restart=always \
               -e CLOUDWAY_DOMAIN=$CLOUDWAY_DOMAIN \
               -e CONSOLE_URL=$CONSOLE_URL \
               -v /var/run/docker.sock:/var/run/docker.sock:ro \
               -v cloudway-data:/data \
               -p 80:80 -p 7999:7999 -p 2200:2200 \
               icloudway/platform:${CLOUDWAY_TAG}
    ```

4. 设置DNS服务器, 推荐使用`dnsmasq`, 修改`/etc/dnsmasq.conf`, 添加以下配置并重启`dnsmasq`:

    ```
    address=/example.com/192.168.99.100
    ```

   其中`example.com`是在运行脚本中设置的根域名, `192.168.99.100`是docker主机的IP地址,
   该配置将设置一个通配域名`*.example.com`。

   在客户机中修改DNS设置, 将DNS服务器地址指向`dnsmasq`所在主机的IP地址。

5. 使用浏览器打开`http://api.example.com`, 点击注册创建新的帐号。以新建帐号登录,
按提示设置名字空间并上传SSH公共密钥, 开始创建应用。
