#!/bin/bash

docker build --tag reportinsert .

docker run --volumes-from report-data --link cirrus-postgres:postgres reportinsert /bin/bash /report-insert.sh
