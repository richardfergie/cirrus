#!/bin/bash -x

# This script does a tonne of stuff before checking if a backup needs
# to be restored.
# Better to do this check early and then restore.
#
# New procedure:
# 1. Do the DATA_CONTAINER/OLD_CONTAINER dance
# 2. Get server running
# 3. Check for web user
# 4. If this user exists, assume all else is good
# 5. If not then restore backup
# 6. Update passwords
# 7. Do schema update

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
# at this point we assume database server is running

webuser=$(docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -t -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
SELECT 1 FROM pg_roles WHERE rolname='web'
EOF
       )

# the BIG assumption here is that if the 'web' account doesn't exist then the whole
# db *can/should* be restored. Maybe delete existing to enable this?
#if [ -z "$webuser" ];
#then
#    echo "web user doesn't exist"
#    echo "restoring from backup"
#    docker run --label "type=tmp" --link cirrus-postgres:postgres -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID -e AWS_SECRET_ACCESS_KEY=${AWS_SECRET_ACCESS_KEY} -e POSTGRES_BACKUP_S3_BUCKET=${POSTGRES_BACKUP_S3_BUCKET} -e PGPASSWORD=${POSTGRES_PG_PASSWORD} backup /restore-database.sh
#else
#    echo "Web user exists"
#    echo "Not restoring backup"
#fi

docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
ALTER ROLE web WITH PASSWORD '${WEB_PG_PASSWORD}';
EOF

docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
CREATE EXTENSION IF NOT EXISTS dblink;
EOF

docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres' <<-EOF
DO
\$do\$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_database WHERE datname = 'admin') THEN
   PERFORM dblink_exec('dbname=' || current_database()  -- current db
                     , 'CREATE DATABASE admin');
  END IF;
END
\$do\$
EOF

cd sql/admin

latest=$(docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres -t --no-align admin' <<-EOF
SELECT file FROM database_schema ORDER BY id DESC LIMIT 1
EOF
      )

echo "Latest file is $latest"

for f in *.sql
do
    if [[ "$f" > "$latest" ]];
    then
        echo "Running $f"
        cat $f | docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres admin'
    else
        echo "Skipping $f"
    fi
done

cd ../adwords
# now for each account do Adwords db stuff
accounts=$(docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres -t --no-align admin' <<-EOF
SELECT dbname FROM adwords_account
EOF
        )

for account in $accounts
do
    for f in *.sql
    do
        if [[ "$f" > "$latest" ]];
    then
        echo "Running $f"
        cat $f | docker run -i --label "type=tmp" --link cirrus-postgres:postgres -e PGPASSWORD=${POSTGRES_PG_PASSWORD} --rm "postgres:9.4" sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres $account'
    else
        echo "Skipping $f"
        fi
    done
done
