#!/bin/bash -xe
source secrets.sh

deploy_backup () {
    tar -zcvf /tmp/backup.tar.gz backup/*
    scp /tmp/backup.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/
    # backup docker image must be present for database deploy
    ssh $PRODUCTION_USER@$PRODUCTION_HOST "tar xvzf /tmp/backup.tar.gz && cd backup && docker build --tag backup ."
}

deploy_database () {
    tar -zcvf /tmp/database.tar.gz database/*
    scp /tmp/database.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/
    # Start/restart database
    ssh $PRODUCTION_USER@$PRODUCTION_HOST "tar xvzf /tmp/database.tar.gz && cd database && ./run.sh"
}

deploy_download () {
    tar -zcvf /tmp/report-download.tar.gz report-download/*
    scp /tmp/report-download.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/
    ssh $PRODUCTION_USER@$PRODUCTION_HOST "tar xvzf /tmp/report-download.tar.gz && cd report-download && docker build --tag report-downloader ."
}

deploy_insert () {
    tar -zcvf /tmp/report-insert.tar.gz report-insert/*
    scp /tmp/report-insert.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/
    ssh $PRODUCTION_USER@$PRODUCTION_HOST "tar xvzf /tmp/report-insert.tar.gz && cd report-insert &&  docker build --tag reportinsert ."
}

deploy_cron () {
    scp daily-cron.sh $PRODUCTION_USER@$PRODUCTION_HOST:~/
}

deploy_notebook () {
    tar -zcvf /tmp/jupyter-env.tar.gz jupyter-env/*
    scp /tmp/jupyter-env.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/
    ssh $PRODUCTION_USER@$PRODUCTION_HOST "tar xvzf /tmp/jupyter-env.tar.gz && cd jupyter-env && docker build --tag notebook ."
}

deploy_web () {
    cd web
    ./build.sh
    tar -zcvf /tmp/web.tar.gz deploy/*
    scp /tmp/web.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/
    ssh $PRODUCTION_USER@$PRODUCTION_HOST "mkdir -p web && tar xvzf /tmp/web.tar.gz -C web --strip-components 1 && cd web && ./run.sh"
}

deploy_backup
deploy_database
deploy_download
deploy_insert
deploy_cron
deploy_notebook
deploy_web
