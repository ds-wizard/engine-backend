#!/bin/bash

CONFIGS=`find engine-$COMPONENT/config -name "*.example"`
for CONFIG in $CONFIGS
do
  TARGET=`echo $CONFIG | sed "s/.example//"`
  cp $CONFIG $TARGET
done
