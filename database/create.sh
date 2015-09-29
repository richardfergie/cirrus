#!/bin/bash

export PGUSER=postgres
psql <<- EOSQL
    CREATE USER web;
    CREATE DATABASE web;
    GRANT ALL PRIVILEGES ON DATABASE web TO web;
    CREATE USER adwords;
    CREATE DATABASE adwords;
    GRANT ALL PRIVILEGES ON DATABASE adwords TO adwords;
    \connect adwords
    GRANT SELECT ON ALL TABLES IN SCHEMA public TO web;
    ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO web;
EOSQL

echo "ALTER ROLE web WITH PASSWORD '${WEBPASSWD-web}';" | psql
echo "ALTER ROLE adwords WITH PASSWORD '${ADWPASSWD-adwords}';" | psql
