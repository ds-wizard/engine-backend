#!/bin/bash
#
# This file is supposed to be used in Docker, it is used to prepare 
# various optional stuff and then runs engine-wizard (backend)

# Set locales to UTF-8
export LC_ALL=C.UTF-8
export LANG=C.UTF-8

# Template loading
if [[ $ENABLE_TEMPLATE_LOAD == "1" ]]; then
    echo "Initiating loading templates from filesystem"
    (sleep 60 && python3 load-templates.py /application/engine-wizard/templates http://localhost:3000 $SERVICE_TOKEN) &
fi

# Start engine-wizard
/application/engine-wizard-bin
