#!/bin/bash -xe
echo $(git log --pretty=format:'%H' -n 1) > version
echo $(date) >> version

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
