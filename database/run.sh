#!/bin/bash -xe
OLD_CONTAINER=$(docker ps -a | grep "cirrus-postgres" | awk {'print $1'} | head -1)
DATA_CONTAINER=$(docker ps -a | grep "postgres-data" | awk {'print $1'} | head -1)
echo "Running Build"
docker build --tag cirrusdb .

if [ ! -z "$DATA_CONTAINER" ]
  then
      echo "Data container is present"
  else
      docker create -v /var/lib/postgresql/data --name postgres-data busybox
fi

if [ ! -z "$OLD_CONTAINER" ]
    then
        docker stop $OLD_CONTAINER
        docker rm $OLD_CONTAINER
    else
        echo "No old container"
fi

docker run -d --name cirrus-postgres --volumes-from postgres-data -e POSTGRES_PASSWORD=${POSTGRES_PASSWORD-postgres} cirrusdb

