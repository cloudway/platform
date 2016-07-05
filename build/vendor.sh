#!/usr/bin/env bash
set -e

# this script is used to update vendored dependencies
#
# Usage:
# vendor.sh
#   revendor all dependencies
# vendor.sh github.com/docker/engine-api
#   revendor only the engine-api dependency
# vendor.sh github.com/docker/engine-api v0.3.3
#   vendor only engine-api at the specified tag/commit
# vendor.sh git github.com/docker/engine-api v0.3.3
#   is the same but specifies the VCS for cases where the VCS is something else than git
# vendor.sh git golang/x/sys eb2c74142fd19a79b3f237334c7384d5167b1b46 https://github.com/golang/sys.git
#   vendor only golang.org/x/sys downloading from the specifeid URL

cd "$(dirname "$BASH_SOURCE")/.."
source 'build/.vendor-helpers.sh'

case $# in
0)
    rm -rf vendor/
    ;;
# If user passed arguments to the script
1)
    eval "$(grep -E "^clone [^ ]+ $1" "$0")"
    exit 0
    ;;
2)
    rm -rf "vendor/src/$1"
    clone git "$1" "$2"
    clean
    exit 0
    ;;
[34])
    rm -rf "vendor/src/$2"
    clone "$@"
    clean
    exit 0
    ;;
*)
    >&2 echo "error: unexpected parameters"
    exit 1
    ;;
esac

# the following lines are in sorted order, FYI
clone git github.com/aarondl/tpl e4905e745b4e2371caa002edf3ccdaf798ffd4b4
clone git github.com/dgrijalva/jwt-go d2709f9f1f31ebcda9651b03077758c1f3a0018c
clone git github.com/docker/distribution 596ca8b86acd3feebedae6bc08abf2a48d403a14
clone git github.com/docker/engine-api f90ecdb1e989f834dabbd91807e891094aa069fe
clone git github.com/docker/go-connections c7838b258fbfa3fe88eecfb2a0e08ea0dbd6a646
clone git github.com/docker/go-units 09dda9d4b0d748c57c14048906d3d094a58ec0c9
clone git github.com/garyburd/redigo b8dc90050f24c1a73a52f107f3f575be67b21b7c
clone git github.com/gorilla/context aed02d124ae4a0e94fea4541c8effd05bf0c8296
clone git github.com/gorilla/mux 9fa818a44c2bf1396a17f9d5a3c0f6dd39d2ff8e
clone git github.com/gorilla/securecookie ff356348f74133a59d3e93aa24b5b4551b6fe90d
clone git github.com/gorilla/sessions 56ba4b0a11da87516629a57408a5f7e4c8ea7b0b
clone git github.com/justinas/nosurf 2e708f28095ba17463e41438bbfd53abae8b6794
clone git github.com/opencontainers/runc 8e22b1d36b2ec794e16fb47cf662c50e2553cb9f
clone git github.com/oxtoacart/bpool 4e1c5567d7c2dd59fa4c7c83d34c2f3528b025d6
clone git github.com/Sirupsen/logrus v0.10.0
clone git github.com/Microsoft/go-winio v0.3.4
clone git golang.org/x/crypto 5bcd134fee4dd1475da17714aac19c0aa0142e2f https://github.com/golang/crypto.git
clone git golang.org/x/net 30db96677b74e24b967e23f911eb3364fc61a011 https://github.com/golang/net.git
clone git golang.org/x/oauth2 65a8d08c6292395d47053be10b3c5e91960def76 https://github.com/golang/oauth2.git
clone git golang.org/x/sys eb2c74142fd19a79b3f237334c7384d5167b1b46 https://github.com/golang/sys.git
clone git gopkg.in/authboss.v0 586415a7db9d2b1538cd2c05ca2dbbce0ee9cc62
clone git gopkg.in/mgo.v2 29cc868a5ca65f401ff318143f9408d02f4799cc
clone git gopkg.in/yaml.v2 a83829b6f1293c91addabc89d0571c246397bbf4
clean
