ALTER TABLE keyword_attributes ADD COLUMN destination_url TEXT;

INSERT INTO database_schema(description,file) VALUES ('Add keyword destination url','0001-destinationurl.sql');
