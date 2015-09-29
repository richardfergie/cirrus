#!/bin/bash -xe

OLD_CONTAINER=$(sudo docker ps | grep "0.0.0.0:80" | awk {'print $1'} | head -1)
echo "Running Build"
sudo docker build --tag cirrusweb .
if [ ! -z "$OLD_CONTAINER" ]
    then
        sudo docker stop $OLD_CONTAINER
        sudo docker rm $OLD_CONTAINER
    else
        echo "No old container"
fi

docker run -p 80:3000 --link cirrus-postgres:postgres --rm cirrusweb ./web

