#!/bin/bash -xe

docker build --tag backup .
source ../secrets.sh

docker run --volumes-from report-data -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID -e AWS_SECRET_ACCESS_KEY=${AWS_SECRET_ACCESS_KEY} -e ADWORDS_REPORT_S3_BUCKET=${ADWORDS_REPORT_S3_BUCKET} backup /report-backup.sh

docker run --link cirrus-postgres:postgres -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID -e AWS_SECRET_ACCESS_KEY=${AWS_SECRET_ACCESS_KEY} -e POSTGRES_BACKUP_S3_BUCKET=${POSTGRES_BACKUP_S3_BUCKET} -e PGPASSWORD=${ADWORDS_PG_PASSWORD} backup /db-backup.sh
