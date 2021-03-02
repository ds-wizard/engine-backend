#!/bin/bash
#
# This file is supposed to be used in Docker, it is used to prepare 
# various optional stuff and then runs engine-wizard (backend)

# Set locales to UTF-8
export LC_ALL=C.UTF-8
export LANG=C.UTF-8

# Start engine-wizard
/application/engine-wizard-bin
