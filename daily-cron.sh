#!/bin/bash -xe

source ~/secrets.sh
cd ~/report-download && ./run.sh && cd ~/report-insert && ./run.sh
cd ~/backup && ./run.sh
# clean old reports
docker run --volumes-from report-data "postgres:9.4" sh -c "find /opt/reports/* -mtime +7 -exec rm -v {} \;"
