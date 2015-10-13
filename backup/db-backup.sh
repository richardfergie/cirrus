#!/bin/bash -xe

ADWORDS_FILE=`date +"%Y-%m-%d"`.Adwords.sql
COMPRESSED_FILE=${ADWORDS_FILE}.gz

pg_dump -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U adwords -f /tmp/${ADWORDS_FILE}

gzip -c -9 /tmp/${ADWORDS_FILE} > /tmp/${COMPRESSED_FILE}

aws s3 cp /tmp/${COMPRESSED_FILE} s3://${POSTGRES_BACKUP_S3_BUCKET}/${COMPRESSED_FILE} --region=us-east-1
