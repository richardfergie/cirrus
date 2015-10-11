#!/bin/bash -xe
source ../secrets.sh
docker build --tag migratedb .

docker run -e ADWPASSWD=${ADWORDS_PG_PASSWORD} --link cirrus-postgres:postgres migratedb /dbmigrate
