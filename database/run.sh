#!/bin/bash -xe
OLD_CONTAINER=$(docker ps -a | grep "cirrus-postgres" | awk {'print $1'} | head -1)
DATA_CONTAINER=$(docker ps -a | grep "postgres-data" | awk {'print $1'} | head -1)

source ../secrets.sh

if [ ! -z "$DATA_CONTAINER" ]
  then
      echo "Data container is present"
  else
      docker create -v /var/lib/postgresql/data --name postgres-data --label "type=data" busybox
fi

if [ ! -z "$OLD_CONTAINER" ]
    then
        docker stop $OLD_CONTAINER
        docker rm $OLD_CONTAINER
    else
        echo "No old container"
fi

docker run -d --label "type=database" --name cirrus-postgres --volumes-from postgres-data -e POSTGRES_PASSWORD=${POSTGRES_PG_PASSWORD} "postgres:9.4"

# not ideal, but better solutions are complicated. 5 seconds may not be enough for a new install
sleep 5

docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
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

docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
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

docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
ALTER ROLE adwords WITH PASSWORD '${ADWORDS_PG_PASSWORD}';
ALTER ROLE web WITH PASSWORD '${WEB_PG_PASSWORD}';
EOF

docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
CREATE EXTENSION IF NOT EXISTS dblink;
EOF

docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
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

docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
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

docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
GRANT ALL PRIVILEGES ON DATABASE web TO web;
GRANT ALL PRIVILEGES ON DATABASE adwords TO adwords;
\connect adwords
GRANT SELECT ON ALL TABLES IN SCHEMA public TO web;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO web;

EOF

tables=$(docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${ADWORDS_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U adwords' <<-EOF
\dt
EOF
)

if [ "$tables" == "No relations found." ];
then
    echo "No relations found - loading backup"
    docker run --label "type=tmp" --link cirrus-postgres:postgres -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID -e AWS_SECRET_ACCESS_KEY=${AWS_SECRET_ACCESS_KEY} -e POSTGRES_BACKUP_S3_BUCKET=${POSTGRES_BACKUP_S3_BUCKET} -e PGPASSWORD=${ADWORDS_PG_PASSWORD} backup /restore-database.sh
else
    echo "Data already in place. Sweet"
fi

cd sql

latest=$(docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${ADWORDS_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U adwords -t --no-align' <<-EOF
SELECT file FROM database_schema ORDER BY id DESC LIMIT 1
EOF
)

echo "Latest file is $latest"

for f in *.sql
do
    if [[ "$f" > "$latest" ]];
    then
        echo "Running $f"
        cat $f | docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${ADWORDS_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U adwords'
    else
        echo "Skipping $f"
    fi
done
