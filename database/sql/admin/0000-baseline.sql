-- updates to this schema are mainly handled by
-- the web application
-- Only changes that the application won't deal with
-- (i.e. destructive changes will be included here)
CREATE TABLE database_schema (
       id SERIAL PRIMARY KEY,
       description TEXT NOT NULL,
       file TEXT NOT NULL,
       timestamp TIMESTAMP DEFAULT now()
       );
INSERT INTO database_schema(description,file) VALUES ('Baseline schema','0000-baseline.sql');
