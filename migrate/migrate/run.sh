#!/bin/bash -xe
docker build --tag migratedb .

docker run --link cirrus-postgres:postgres migratedb /dbmigrate
