#!/bin/bash -xe

source ../../secrets.sh

function jsonValue() {
  KEY=$1
  awk -F"[,:}]" '{for(i=1;i<=NF;i++){if($i~/'$KEY'\042/){print $(i+1)}}}' | tr -d '"'
}

API_VERSION=$(curl -s --unix-socket /var/run/docker.sock http:/version | jsonValue "ApiVersion")
echo "Docker API Version is $API_VERSION"

OLD_CONTAINER=$(docker ps | grep "0.0.0.0:80" | awk {'print $1'} | head -1)
echo "Running Build"
docker build --tag cirruswww .
if [ ! -z "$OLD_CONTAINER" ]
    then
      sudo docker stop $OLD_CONTAINER
    else
      echo "No old container"
fi
sudo docker run -d -p 80:3000 -v /var/run/docker.sock:/var/run/docker.sock --link cirrus-postgres:postgres -e PGPASS=${WEB_PG_PASSWORD} -e DOCKER_API_VERSION=${API_VERSION} -e POSTGRES_PG_PASSWORD=${POSTGRES_PG_PASSWORD} -e ADWORDS_CLIENT_SECRET=${ADWORDS_CLIENT_SECRET} -e ADWORDS_DEVELOPER_TOKEN=${ADWORDS_DEVELOPER_TOKEN} -e ADWORDS_REFRESH_TOKEN=${ADWORDS_REFRESH_TOKEN} -e ADWORDS_CLIENT_ID=${ADWORDS_CLIENT_ID} cirruswww ./jupyter-proxy config/settings.yml

