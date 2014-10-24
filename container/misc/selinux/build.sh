#!/bin/bash -e

BASE_DIR=${BASE_DIR-$(cd $(dirname $0)/../..; pwd)}

pushd $BASE_DIR/misc/selinux > /dev/null
make -f /usr/share/selinux/devel/Makefile
bzip2 -9 cloudway.pp
mkdir -p $BASE_DIR/target/selinux
mv cloudway.pp.bz2 $BASE_DIR/target/selinux
rm -rf tmp
popd > /dev/null
