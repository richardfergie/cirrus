FROM ubuntu

RUN apt-get update && apt-get install -q -y awscli postgresql-client

ADD report-backup.sh /report-backup.sh
ADD db-backup.sh /db-backup.sh