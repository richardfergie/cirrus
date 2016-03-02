#!/bin/bash

docker build --tag reportinsert .
source ../secrets.sh
docker run --label "type=tmp" --volumes-from report-data --link cirrus-postgres:postgres -e PGPASSWORD=${ADWORDS_PG_PASSWORD} reportinsert /report-insert.sh
