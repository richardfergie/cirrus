#!/bin/bash -xe
source secrets.sh

tar -zcvf /tmp/database.tar.gz database/*
scp /tmp/database.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/

ssh $PRODUCTION_USER@$PRODUCTION_HOST "tar xvzf /tmp/database.tar.gz && cd database && ./run.sh"

cd migrate
./build.sh

tar -zcvf /tmp/dbmigrate.tar.gz migrate/*
scp /tmp/dbmigrate.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/

ssh $PRODUCTION_USER@$PRODUCTION_HOST "tar xvzf /tmp/dbmigrate.tar.gz && cd migrate && ./run.sh"

cd ..
tar -zcvf /tmp/report-download.tar.gz report-download/*
tar -zcvf /tmp/report-insert.tar.gz report-insert/*
tar -zcvf /tmp/backup.tar.gz backup/*

scp /tmp/report-download.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/
scp /tmp/report-insert.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/
scp /tmp/backup.tar.gz $PRODUCTION_USER@$PRODUCTION_HOST:/tmp/

ssh $PRODUCTION_USER@$PRODUCTION_HOST "tar xvzf /tmp/report-download.tar.gz && tar xvzf /tmp/report-insert.tar.gz && tar xvzf /tmp/backup.tar.gz"

scp daily-cron.sh $PRODUCTION_USER@$PRODUCTION_HOST:~/

