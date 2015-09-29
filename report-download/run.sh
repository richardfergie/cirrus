#!/bin/bash -xe
docker build --tag report-downloader .
source ../secrets.sh
DATA_CONTAINER=$(docker ps -a | grep "report-data" | awk {'print $1'} | head -1)

if [ ! -z "$DATA_CONTAINER" ]
  then
      echo "Data container is present"
  else
      echo "Creating data container"
      docker create -v /opt/reports --name report-data busybox
fi

docker run --volumes-from report-data -e ADWORDS_CLIENT_ID=${ADWORDS_CLIENTID} -e ADWORDS_CLIENT_SECRET=${ADWORDS_CLIENT_SECRET} -e ADWORDS_DEVELOPER_TOKEN=${ADWORDS_DEVELOPER_TOKEN} -e ADWORDS_REFRESH_TOKEN=${ADWORDS_REFRESH_TOKEN} downloader python /download.py
