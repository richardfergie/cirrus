#!/bin/bash -xe

source ~/secrets.sh
echo "Running report download/insert"
cd ~/report-download && ./run.sh && cd ~/report-insert && ./run.sh && echo "Download complete"
echo "Running backup"
cd ~/backup && ./run.sh && echo "Backup complete"
echo "Cleaning old reports"
docker run --volumes-from report-data "postgres:9.4" sh -c "find /opt/reports/* -mtime +7 -exec rm -v {} \;"
