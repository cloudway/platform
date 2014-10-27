#!/bin/bash -e

if [ -z "$JAVACMD" ]; then
  if [ -n "$JAVA_HOME" ]; then
    if [ -x "$JAVA_HOME/jre/sh/java" ]; then
      JAVACMD="$JAVA_HOME/jre/sh/java"
    else
      JAVACMD="$JAVA_HOME/bin/java"
    fi
  else
    JAVACMD=$(which java)
  fi
fi

if [ ! -x "$JAVACMD" ]; then
  echo "Error: JAVA_HOME is not defined correctly." 1>&2
  exit 1
fi

max_memory_mb=$CLOUDWAY_MEMORY_LIMIT
if ! [[ "$max_memory_mb" =~ ^[0-9]+$ ]]; then
  max_memory_mb=512
fi
if [ -z $JVM_HEAP_RATIO ]; then
  JVM_HEAP_RATIO=0.5
fi
max_heap=$(echo "$max_memory_mb * $JVM_HEAP_RATIO" | bc | awk '{ print int($1+0.5) }')

JAVA_OPTS="-Xmx${max_heap}m -XX:+AggressiveOpts -Dfile.encoding=UTF-8 $JAVA_OPTS"

echo $$ > ${CLOUDWAY_CWAS_DIR}/run/cwas.pid
exec $JAVACMD $JAVA_OPTS -Dcloudway.server.root=${CLOUDWAY_CWAS_DIR} \
    -Djava.util.logging.config.file=${CLOUDWAY_CWAS_DIR}/conf/logging.conf \
    -jar ${CLOUDWAY_CWAS_DIR}/lib/launcher.jar
