#!/bin/bash

CONFIGS=`find $COMPONENT-server/config -name "*.example"`
for CONFIG in $CONFIGS
do
  TARGET=`echo $CONFIG | sed "s/.example//"`
  cp $CONFIG $TARGET
done
