#!/bin/bash

set -x

sbt assembly

TIME="$(date +%s)"

WORKING_DIR="`pwd`"
JAR_PATH="$WORKING_DIR/target/scala-2.12/csbt-assembly-0.1-SNAPSHOT.jar"

echo $JAR_PATH

echo "#!/bin/sh" > /usr/local/bin/csbt
echo "java -jar $JAR_PATH \$@" >> /usr/local/bin/csbt

chmod +x /usr/local/bin/csbt
