CREATE TABLE adwords_account (id SERIAL PRIMARY KEY,
                              client_id VARCHAR NOT NULL,
                              dbname VARCHAR NOT NULL,
                              dbuser VARCHAR NOT NULL,
                              dbpassword VARCHAR NOT NULL,
                              description VARCHAR NOT NULL,
                              created TIMESTAMP DEFAULT now()
                             );
CREATE TABLE database_schema (
       id SERIAL PRIMARY KEY,
       description TEXT NOT NULL,
       file TEXT NOT NULL,
       timestamp TIMESTAMP DEFAULT now()
       );
INSERT INTO database_schema(description,file) VALUES ('Baseline schema','0000-baseline.sql');
