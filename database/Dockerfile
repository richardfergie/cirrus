FROM postgres:9.4

RUN mkdir -p /docker-entrypoint-initdb.d

ADD create.sh /docker-entrypoint-initdb.d/create.sh
RUN chmod 755 /docker-entrypoint-initdb.d/create.sh