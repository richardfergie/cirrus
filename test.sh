#!/bin/bash

# Uses production data - which is bad
# Initial schema update has inconsequential errors
#   which the script just steamrollers past
#   - which is bad
# But this is better than nothing

source secrets.sh

ip addr show eth0 | grep ${PRODUCTION_HOST}

if [[ $? == 1 ]]
then
    echo "Not production -- Running tests"
    docker rm postgres-data
    docker rm report-data
    cd database && ./run.sh
    cd ../report-download && ./run.sh
    cd ../report-insert && ./run.sh
fi
