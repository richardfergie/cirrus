#!/bin/bash -xe
docker build --tag report-downloader .
source ../secrets.sh
DATA_CONTAINER=$(docker ps -a | grep "report-data" | awk {'print $1'} | head -1)

if [ ! -z "$DATA_CONTAINER" ]
  then
      echo "Data container is present"
  else
      echo "Creating data container"
      docker create -v /opt/reports/ --name report-data busybox
fi

OLD_CONTAINER=$(docker ps -a | grep "adwords-download" | awk {'print $1'} | head -1)

if [ ! -z "$OLD_CONTAINER" ]
    then
        docker stop $OLD_CONTAINER
        docker rm $OLD_CONTAINER
    else
        echo "No old container"
fi

docker run --name adwords-download --volumes-from report-data -e ADWORDS_CLIENT_ID=${ADWORDS_CLIENTID} -e ADWORDS_CLIENT_SECRET=${ADWORDS_CLIENT_SECRET} -e ADWORDS_DEVELOPER_TOKEN=${ADWORDS_DEVELOPER_TOKEN} -e ADWORDS_REFRESH_TOKEN=${ADWORDS_REFRESH_TOKEN} report-downloader python /download.py
