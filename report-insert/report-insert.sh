#!/bin/bash -e
TODAY=`date +"%Y-%m-%d"`

DBNAME=$(psql -t -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres web<<-EOF
SELECT database.dbname
       FROM account
       JOIN database
       ON account.database = database.id
       WHERE account.client_id = '${CLIENT_ID}'
       LIMIT 1 --only a problem if client id occurs multiple times
EOF
)

function updateAccountDatabase {
    #assumes that the reports have all been downloaded
    ACCOUNTID=$1
    DB_NAME=$2
    psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres "$DB_NAME"<<- EOF
  ALTER TABLE text_ad_performance ALTER COLUMN account_id SET DEFAULT '$ACCOUNTID';
  \copy text_ad_performance(campaign_id,ad_group_id,ad_id,keyword_id,day,network,clicks,impressions,cost,avgpos,conversions) FROM PROGRAM 'zcat /opt/reports/$ACCOUNTID:TextAdPerformance:$TODAY' CSV HEADER
  ALTER TABLE text_ad_performance ALTER COLUMN account_id DROP DEFAULT;
  \echo TextAdPerformance

  ALTER TABLE ad_group_performance ALTER COLUMN account_id SET DEFAULT '$ACCOUNTID';
  \copy ad_group_performance(campaign_id,ad_group_id,day,network,clicks,impressions,cost,avgpos,conversions) FROM PROGRAM 'zcat /opt/reports/$ACCOUNTID:AdGroupPerformance:$TODAY' CSV HEADER
  ALTER TABLE ad_group_performance ALTER COLUMN account_id DROP DEFAULT;
  \echo AdGroupPerformance

  ALTER TABLE campaign_performance ALTER COLUMN account_id SET DEFAULT '$ACCOUNTID';
  \copy campaign_performance(campaign_id,day,network,clicks,impressions,cost,avgpos,conversions) FROM PROGRAM 'zcat /opt/reports/$ACCOUNTID:CampaignPerformance:$TODAY' CSV HEADER
  ALTER TABLE campaign_performance ALTER COLUMN account_id DROP DEFAULT;
  \echo CampaignPerformance

  CREATE TEMP TABLE campaign_attributes_temp(campaign_id BIGINT, status TEXT, day DATE, impressions BIGINT);
  \copy campaign_attributes_temp(campaign_id,status,day,impressions) FROM PROGRAM 'zcat /opt/reports/$ACCOUNTID:CampaignAttribute:$TODAY' CSV HEADER
  ALTER TABLE campaign_attributes ALTER COLUMN account_id SET DEFAULT '$ACCOUNTID';
  INSERT INTO campaign_attributes(campaign_id,status) (SELECT campaign_id,status FROM campaign_attributes_temp);
  ALTER TABLE campaign_attributes ALTER COLUMN account_id DROP DEFAULT;
  \echo CampaignAttributes

  CREATE TEMP TABLE ad_group_attributes_temp(campaign_id BIGINT, ad_group_id BIGINT, status TEXT, cpc_bid BIGINT, day DATE, impressions BIGINT);
  \copy ad_group_attributes_temp(campaign_id,ad_group_id,status,cpc_bid,day,impressions) FROM PROGRAM 'zcat /opt/reports/$ACCOUNTID:AdGroupAttribute:$TODAY' CSV HEADER
  ALTER TABLE ad_group_attributes ALTER COLUMN account_id SET DEFAULT '$ACCOUNTID';
  INSERT INTO ad_group_attributes(campaign_id,ad_group_id,status,cpc_bid) (SELECT campaign_id,ad_group_id,status,cpc_bid FROM ad_group_attributes_temp);
  ALTER TABLE ad_group_attributes ALTER COLUMN account_id DROP DEFAULT;
  \echo AdGroupAttributes

  CREATE TEMP TABLE text_ad_attributes_temp(campaign_id BIGINT, ad_group_id BIGINT, ad_id BIGINT, status TEXT, day DATE, impressions BIGINT);
  \copy text_ad_attributes_temp(campaign_id,ad_group_id,ad_id,status,day,impressions) FROM PROGRAM 'zcat /opt/reports/$ACCOUNTID:TextAdAttribute:$TODAY' CSV HEADER
  ALTER TABLE text_ad_attributes ALTER COLUMN account_id SET DEFAULT '$ACCOUNTID';
  INSERT INTO text_ad_attributes(campaign_id,ad_group_id,ad_id,status) (SELECT campaign_id,ad_group_id,ad_id,status FROM text_ad_attributes_temp);
  ALTER TABLE text_ad_attributes ALTER COLUMN account_id DROP DEFAULT;
  \echo TextAdAttributes

  CREATE TEMP TABLE keyword_attributes_temp(campaign_id BIGINT, ad_group_id BIGINT, keyword_id BIGINT, bid BIGINT, quality_score BIGINT, first_page_bid BIGINT, top_of_page_bid BIGINT, destination_url TEXT, finalurls TEXT, day DATE, impressions BIGINT);
  \copy keyword_attributes_temp(campaign_id,ad_group_id,keyword_id,bid,quality_score,first_page_bid,top_of_page_bid,destination_url,finalurls, day,impressions) FROM PROGRAM 'zcat /opt/reports/$ACCOUNTID:KeywordAttribute:$TODAY' CSV HEADER
  ALTER TABLE keyword_attributes ALTER COLUMN account_id SET DEFAULT '$ACCOUNTID';
  INSERT INTO keyword_attributes(campaign_id,
                                 ad_group_id,
                                 keyword_id,
                                 bid,
                                 quality_score,
                                 first_page_bid,
                                 top_of_page_bid,
                                 destination_url) (SELECT campaign_id,
                                                          ad_group_id,
                                                          keyword_id,
                                                          bid,
                                                          quality_score,
                                                          first_page_bid,
                                                          top_of_page_bid,
                                                          COALESCE(destination_url,finalurls)
                                                          FROM keyword_attributes_temp);
  ALTER TABLE keyword_attributes ALTER COLUMN account_id DROP DEFAULT;
  \echo KeywordAttributes

  CREATE TEMP TABLE campaign_structure_temp(campaign_id BIGINT,name TEXT,impressions BIGINT);
  \copy campaign_structure_temp(campaign_id,name,impressions) FROM PROGRAM 'zcat /opt/reports/$ACCOUNTID:CampaignStructure:$TODAY' CSV HEADER
  ALTER TABLE campaign_structure ALTER COLUMN account_id SET DEFAULT '$ACCOUNTID';
  INSERT INTO campaign_structure(campaign_id,name) (SELECT campaign_id,name FROM campaign_structure_temp WHERE campaign_id NOT IN (SELECT campaign_id FROM campaign_structure));
  UPDATE campaign_structure SET name=new.name FROM (SELECT campaign_id,name FROM campaign_structure_temp) as new WHERE new.campaign_id = campaign_structure.id;
  ALTER TABLE campaign_structure ALTER COLUMN account_id DROP DEFAULT;
  \echo CampaignStructure

  CREATE TEMP TABLE ad_group_structure_temp(campaign_id BIGINT,ad_group_id BIGINT, name TEXT, impressions BIGINT);
  \copy ad_group_structure_temp(campaign_id,ad_group_id,name,impressions) FROM PROGRAM 'zcat /opt/reports/$ACCOUNTID:AdGroupStructure:$TODAY' CSV HEADER
  ALTER TABLE ad_group_structure ALTER COLUMN account_id SET DEFAULT '$ACCOUNTID';
  INSERT INTO ad_group_structure(campaign_id,ad_group_id,name) (SELECT campaign_id,ad_group_id,name FROM ad_group_structure_temp WHERE (campaign_id,ad_group_id) NOT IN (SELECT campaign_id,ad_group_id FROM ad_group_structure));
  UPDATE ad_group_structure SET name=new.name FROM (SELECT campaign_id,ad_group_id,name FROM ad_group_structure_temp) as new WHERE new.campaign_id = ad_group_structure.id AND new.ad_group_id = ad_group_structure.ad_group_id;
  ALTER TABLE ad_group_structure ALTER COLUMN account_id DROP DEFAULT;
  \echo AdGroupStructure

  CREATE TEMP TABLE text_ad_structure_temp(campaign_id BIGINT,
                                           ad_group_id BIGINT,
                                           ad_id BIGINT,
                                           headline TEXT,
                                           description1 TEXT,
                                           description2 TEXT,
                                           display_url TEXT,
                                           destination_url TEXT,
                                           final_url TEXT,
                                           impressions BIGINT);
  \copy text_ad_structure_temp FROM PROGRAM 'zcat /opt/reports/$ACCOUNTID:TextAdStructure:$TODAY' CSV HEADER
  -- text ads are immutable so just delete from old table and recreate
  ALTER TABLE text_ad_structure ALTER COLUMN account_id SET DEFAULT '$ACCOUNTID';
  DELETE FROM text_ad_structure WHERE account_id = '$ACCOUNTID';
  INSERT INTO text_ad_structure(campaign_id,
                                ad_group_id,
                                ad_id,
                                headline,
                                description1,
                                description2,
                                display_url,
                                destination_url) (SELECT campaign_id,
                                                         ad_group_id,
                                                         ad_id,
                                                         headline,
                                                         description1,
                                                         description2,
                                                         display_url,
                                                         COALESCE(destination_url,final_url)
                                                         FROM text_ad_structure_temp);
  ALTER TABLE text_ad_structure ALTER COLUMN account_id DROP DEFAULT;
  \echo TextAdStructure

  CREATE TEMP TABLE keyword_structure_temp(campaign_id BIGINT,
                                           ad_group_id BIGINT,
                                           keyword_id BIGINT,
                                           keyword TEXT,
                                           match_type TEXT,
                                           impressions BIGINT);
  \copy keyword_structure_temp FROM PROGRAM 'zcat /opt/reports/$ACCOUNTID:KeywordStructure:$TODAY' CSV HEADER
  ALTER TABLE keyword_structure ALTER COLUMN account_id SET DEFAULT '$ACCOUNTID';
  DELETE FROM keyword_structure WHERE account_id = '$ACCOUNTID';
  INSERT INTO keyword_structure(campaign_id,
                                ad_group_id,
                                keyword_id,
                                keyword,
                                match_type) (SELECT campaign_id,
                                                    ad_group_id,
                                                    keyword_id,
                                                    keyword,
                                                    match_type
                                                    FROM keyword_structure_temp);
  ALTER TABLE keyword_structure ALTER COLUMN account_id DROP DEFAULT;
  \echo KeywordStructure

EOF
}

updateAccountDatabase ${CLIENT_ID} ${DBNAME}
