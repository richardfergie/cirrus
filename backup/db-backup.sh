#!/bin/bash -xe

COMPRESSED_FILE=`date +"%Y-%m-%d"`.all.sql.gz

# pg_dumpall not great here.
# Can do better with #pg_dumpall --globals only to get global stuff
# Then pg_dump with custom format on individual databases
# But when number of databases is variable this gets more complicated
# than necessary at the moment
pg_dumpall -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres | gzip -c -9 > /tmp/$COMPRESSED_FILE

aws s3 cp /tmp/${COMPRESSED_FILE} s3://${POSTGRES_BACKUP_S3_BUCKET}/${COMPRESSED_FILE} --region=us-east-1
