#!/bin/bash -xe
source secrets.sh

deploy_backup () {
    tar -zcvf /tmp/backup.tar.gz backup/*
    scp /tmp/backup.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/
    # backup docker image must be present for database deplot
    ssh $PRODUCTION_USER@$PRODUCTION_HOST "tar xvzf /tmp/backup.tar.gz && cd backup && docker build --tag backup ."
}

deploy_database () {
    tar -zcvf /tmp/database.tar.gz database/*
    scp /tmp/database.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/
    # Start/restart database
    ssh $PRODUCTION_USER@$PRODUCTION_HOST "tar xvzf /tmp/database.tar.gz && cd database && ./run.sh"
}

deploy_migrate () {
    cd migrate
    ./build.sh
    tar -zcvf /tmp/dbmigrate.tar.gz migrate/*
    scp /tmp/dbmigrate.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/
    ssh $PRODUCTION_USER@$PRODUCTION_HOST "tar xvzf /tmp/dbmigrate.tar.gz && cd migrate && ./run.sh"
    cd ..
}

deploy_download () {
    tar -zcvf /tmp/report-download.tar.gz report-download/*
    scp /tmp/report-download.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/
    ssh $PRODUCTION_USER@$PRODUCTION_HOST "tar xvzf /tmp/report-download.tar.gz"
}

deploy_insert () {
    tar -zcvf /tmp/report-insert.tar.gz report-insert/*
    scp /tmp/report-insert.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/
    ssh $PRODUCTION_USER@$PRODUCTION_HOST "tar xvzf /tmp/report-insert.tar.gz"
}

deploy_cron () {
    scp daily-cron.sh $PRODUCTION_USER@$PRODUCTION_HOST:~/
}

deploy_backup
deploy_database
# this way of migrating not helpful right now
#deploy_migrate
deploy_download
deploy_insert
deploy_cron
