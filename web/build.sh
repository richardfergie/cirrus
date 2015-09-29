#!/bin/bash -xe
echo $(git log --pretty=format:'%H' -n 1) > version
echo $(date) >> version
mkdir -p deploy

stack build
cp .stack-work/install/x86_64-linux/lts-3.4/7.10.2/bin/web deploy/
cp version deploy/
mkdir -p deploy/config
cp config/{routes,settings.yml,models,favicon.ico,robots.txt} deploy/config/
cp -R static deploy/static
