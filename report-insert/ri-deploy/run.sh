#!/bin/bash -xe
docker build --tag report-insert .

docker run --volumes-from report-data --link cirrus-postgres:postgres report-insert /report-insert /opt/reports/ 929-872-4012
