#!/bin/bash -e

DBNAME=$1
DBUSER=$2
DBPASS=$3

createuser -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres ${DBUSER}

createdb -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres ${DBNAME} --owner postgres

psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres ${DBNAME} <<-EOF
ALTER USER "${DBUSER}" WITH PASSWORD '${DBPASS}';
GRANT SELECT ON ALL TABLES IN SCHEMA public TO "${DBUSER}";
ALTER DEFAULT PRIVILEGES IN SCHEMA public
   GRANT SELECT ON TABLES TO "${DBUSER}";
EOF

cd /sql/adwords

for f in *.sql
do
    echo "Running $f"
    cat $f | psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres ${DBNAME}
done
