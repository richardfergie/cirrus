#!/bin/bash -xe
docker build --tag report-downloader .
source ../secrets.sh
DATA_CONTAINER=$(docker ps -a | grep "report-data" | awk {'print $1'} | head -1)

if [ ! -z "$DATA_CONTAINER" ]
  then
      echo "Data container is present"
  else
      echo "Creating data container"
      docker create -v /opt/reports/ --name report-data --label "type=data" busybox
fi

OLD_CONTAINER=$(docker ps -a | grep "adwords-download" | awk {'print $1'} | head -1)

if [ ! -z "$OLD_CONTAINER" ]
    then
        docker stop $OLD_CONTAINER
        docker rm $OLD_CONTAINER
    else
        echo "No old container"
fi

IFS=$'\n'

accounts=$(docker run -i --label type=tmp --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm postgres:9.4 sh -c 'exec psql -t -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres admin'<<-EOF
SELECT DISTINCT client_id FROM adwords_account
EOF
        )

for account in $accounts
do
    client_id=$(echo $account | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
    echo "Downloading reports for ${client_id}"
    docker run --label "type=tmp" --name adwords-download --volumes-from report-data -e ADWORDS_CLIENT_ID=${ADWORDS_CLIENT_ID} -e ADWORDS_CLIENT_SECRET=${ADWORDS_CLIENT_SECRET} -e ADWORDS_DEVELOPER_TOKEN=${ADWORDS_DEVELOPER_TOKEN} -e ADWORDS_REFRESH_TOKEN=${ADWORDS_REFRESH_TOKEN} -e CLIENT_ID=${client_id} report-downloader python /download.py
    echo "Downloaded reports for ${client_id}"
done
