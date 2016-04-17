#!/bin/bash

docker build --tag reportinsert .
source ../secrets.sh

IFS=$'\n'

accounts=$(docker run -i --label type=tmp --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm postgres:9.4 sh -c 'exec psql -t -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres web'<<-EOF
SELECT DISTINCT client_id FROM account
EOF
        )

for account in $accounts
do
    echo "Inserting reports for $account"
    docker run --label "type=tmp" -t --volumes-from report-data --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} -e CLIENT_ID=${account} reportinsert /report-insert.sh
    echo "Reports for $account inserted"
done
