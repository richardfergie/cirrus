#!/bin/bash -xe
OLD_CONTAINER=$(docker ps -a | grep "cirrus-postgres" | awk {'print $1'} | head -1)
DATA_CONTAINER=$(docker ps -a | grep "postgres-data" | awk {'print $1'} | head -1)
#echo "Running Build"
#docker build --tag cirrusdb .

source ../secrets.sh

if [ ! -z "$DATA_CONTAINER" ]
  then
      echo "Data container is present"
  else
      docker create -v /var/lib/postgresql/data --name postgres-data busybox
fi

if [ ! -z "$OLD_CONTAINER" ]
    then
        docker stop $OLD_CONTAINER
        docker rm $OLD_CONTAINER
    else
        echo "No old container"
fi

docker run -d --name cirrus-postgres --volumes-from postgres-data -e POSTGRES_PASSWORD=${POSTGRES_PG_PASSWORD} "postgres:9.4"

sleep 5 # not ideal, but better solutions are complicated

docker run -i --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
DO
\$do\$
BEGIN
  IF NOT EXISTS (
      SELECT *
      FROM   pg_catalog.pg_user
      WHERE  usename = 'adwords') THEN

      CREATE ROLE adwords NOSUPERUSER LOGIN;
   END IF;
END
\$do\$
EOF

docker run -i --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
DO
\$do\$
BEGIN
  IF NOT EXISTS (
      SELECT *
      FROM   pg_catalog.pg_user
      WHERE  usename = 'web') THEN

      CREATE ROLE web NOSUPERUSER LOGIN;
   END IF;
END
\$do\$
EOF

docker run -i --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
ALTER ROLE adwords WITH PASSWORD '${ADWORDS_PG_PASSWORD}';
ALTER ROLE web WITH PASSWORD '${WEB_PG_PASSWORD}';
EOF

docker run -i --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
CREATE EXTENSION IF NOT EXISTS dblink;
EOF

docker run -i --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
DO
\$do\$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_database WHERE datname = 'web') THEN
   PERFORM dblink_exec('dbname=' || current_database()  -- current db
                     , 'CREATE DATABASE web');
  END IF;
END
\$do\$
EOF

docker run -i --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
DO
\$do\$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_database WHERE datname = 'adwords') THEN
   PERFORM dblink_exec('dbname=' || current_database()  -- current db
                     , 'CREATE DATABASE adwords');
  END IF;
END
\$do\$
EOF

docker run -i --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
GRANT ALL PRIVILEGES ON DATABASE web TO web;
GRANT ALL PRIVILEGES ON DATABASE adwords TO adwords;
\connect adwords
GRANT SELECT ON ALL TABLES IN SCHEMA public TO web;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO web;

EOF
