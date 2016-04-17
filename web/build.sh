#!/bin/bash -xe
echo $(git log --pretty=format:'%H' -n 1) > version
echo $(date) >> version

#sudo docker build --tag=potentiallabbuild .
# next line is needed to get a containerid rather than an image
#sudo docker run potentiallabbuild echo "Image built"
#CONTAINER_ID=$(sudo docker ps -q -n=1) #won't work if another build happens in between
#echo "Docker container " $CONTAINER_ID
#sudo docker cp $CONTAINER_ID:/.cabal/bin/potential-lab deploy/
whoami
touch Settings/StaticFiles.hs
stack setup
stack install
strip .stack-work/docker/_home/.local/bin/jupyter-proxy
cp .stack-work/docker/_home/.local/bin/jupyter-proxy deploy/
cp version deploy/version
mkdir -p deploy/config
cp config/{routes,settings.yml,models,favicon.ico,robots.txt} deploy/config/
cp -R static deploy/
