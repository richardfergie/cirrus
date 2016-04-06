#!/bin/bash -e

SQL_DUMP_FILE=$(aws s3 ls ${POSTGRES_BACKUP_S3_BUCKET} --region us-east-1 | grep all.sql.gz | tail -1 | awk '{print $4}')

if [ ! -z "$SQL_DUMP_FILE" ]
then
    echo "Backup exists."
    echo "Restoring backup ${SQL_DUMP_FILE}"
    aws s3 cp s3://${POSTGRES_BACKUP_S3_BUCKET}/${SQL_DUMP_FILE} /tmp/all.sql.gz --region=us-east-1
    zcat /tmp/adwords.sql.gz | psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres
    echo "Backup restored"
else
    echo "Backup file does not exist"
fi
