#!/bin/bash

set -e

BUILD_INFO_FILE=../config/build-info.yml

# ---------------------------------------------------------------
# 1. Set a name
# ---------------------------------------------------------------
appName="Data Stewardship Wizard Server"
echo "name: $appName" > $BUILD_INFO_FILE

# ---------------------------------------------------------------
# 2. Create app version
#       - if there is a git tag, use it
#       - if there is no git tag, use branch and last commit as a identification of build version
# ---------------------------------------------------------------
branch=`git rev-parse --abbrev-ref HEAD`
commit=`git rev-parse --short HEAD`
appVersion="$branch~$commit"

gittag=`git tag -l --contains HEAD | head -n 1`
if test -n "$gittag"
then
    appVersion="$gittag~$commit"
fi

echo "version: $appVersion" >> $BUILD_INFO_FILE

# ---------------------------------------------------------------
# 3. Create build timestamp
# ---------------------------------------------------------------
builtAtTimestamp=`date +"%Y/%m/%d %TZ"`
echo "builtAt: $builtAtTimestamp" >> $BUILD_INFO_FILE
