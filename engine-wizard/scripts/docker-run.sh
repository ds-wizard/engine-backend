#!/bin/bash
#
# This file is supposed to be used in Docker, it is used to prepare 
# various optional stuff such as cron tasks, starts cron service, 
# and then runs engine-wizard (backend)

# CRON - scheduled jobs
if [[ $ENABLE_CRON == "1" ]]; then
    # Prepare cron dir
    mkdir -p /etc/cron.d/

    # Set cron for feedback sync if requested
    if [[ $ENABLE_CRON_FEEDBACK_SYNC == "1" ]]; then
        if [[ -z $SERVICE_TOKEN || -z $API_URL ]]; then
            echo "Failed to set Feedback sync cron job: SERVICE_TOKEN and API_URL are required"
        else
            echo "0 2 * * *   root   wget --header \"Authorization: Bearer $SERVICE_TOKEN\" \"$API_URL/feedbacks/synchronization\"" > /etc/cron.d/feedback-sync
        fi
    fi

    # Prepare cron jobs (under root's crontab)
    chmod 644 /etc/cron.d/* 2> /dev/null
    crontab /etc/cron.d/* 2> /dev/null
    touch /var/log/cron.log /etc/crontab /etc/cron.*/* 2> /dev/null

    # Start cron daemon
    cron
fi

# Start engine-wizard
/application/engine-wizard-bin
