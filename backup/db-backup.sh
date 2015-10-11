#!/bin/bash -xe

ADWORDS_FILE=`date +"%Y-%m-%d"`.Adwords.sql

pg_dump -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U adwords -f /tmp/${ADWORDS_FILE}

aws s3 cp /tmp/${ADWORDS_FILE} s3://${POSTGRES_BACKUP_S3_BUCKET}/${ADWORDS_FILE}
