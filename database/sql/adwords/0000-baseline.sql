--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: ad_group_attributes; Type: TABLE; Schema: public; Owner: adwords; Tablespace: 
--

CREATE TABLE ad_group_attributes (
    id integer NOT NULL,
    account_id character varying NOT NULL,
    campaign_id bigint NOT NULL,
    ad_group_id bigint NOT NULL,
    status character varying NOT NULL,
    cpc_bid bigint NOT NULL,
    observed timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: ad_group_attributes_id_seq; Type: SEQUENCE; Schema: public;
--

CREATE SEQUENCE ad_group_attributes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: ad_group_attributes_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: adwords
--

ALTER SEQUENCE ad_group_attributes_id_seq OWNED BY ad_group_attributes.id;


--
-- Name: ad_group_performance; Type: TABLE; Schema: public; Owner: adwords; Tablespace: 
--

CREATE TABLE ad_group_performance (
    id integer NOT NULL,
    account_id character varying NOT NULL,
    campaign_id bigint NOT NULL,
    ad_group_id bigint NOT NULL,
    day date NOT NULL,
    network character varying NOT NULL,
    clicks bigint NOT NULL,
    impressions bigint NOT NULL,
    cost bigint NOT NULL,
    avgpos double precision NOT NULL,
    conversions bigint NOT NULL,
    observed timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: ad_group_performance_id_seq; Type: SEQUENCE; Schema: public; Owner: adwords
--

CREATE SEQUENCE ad_group_performance_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ad_group_performance_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: adwords
--

ALTER SEQUENCE ad_group_performance_id_seq OWNED BY ad_group_performance.id;


--
-- Name: ad_group_structure; Type: TABLE; Schema: public; Owner: adwords; Tablespace: 
--

CREATE TABLE ad_group_structure (
    id integer NOT NULL,
    account_id character varying NOT NULL,
    campaign_id bigint NOT NULL,
    ad_group_id bigint NOT NULL,
    name character varying NOT NULL
);


--
-- Name: ad_group_structure_id_seq; Type: SEQUENCE; Schema: public; Owner: adwords
--

CREATE SEQUENCE ad_group_structure_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ad_group_structure_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: adwords
--

ALTER SEQUENCE ad_group_structure_id_seq OWNED BY ad_group_structure.id;


--
-- Name: campaign_attributes; Type: TABLE; Schema: public; Owner: adwords; Tablespace: 
--

CREATE TABLE campaign_attributes (
    id integer NOT NULL,
    account_id character varying NOT NULL,
    campaign_id bigint NOT NULL,
    status character varying NOT NULL,
    observed timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: campaign_attributes_id_seq; Type: SEQUENCE; Schema: public; Owner: adwords
--

CREATE SEQUENCE campaign_attributes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: campaign_attributes_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: adwords
--

ALTER SEQUENCE campaign_attributes_id_seq OWNED BY campaign_attributes.id;


--
-- Name: campaign_performance; Type: TABLE; Schema: public; Owner: adwords; Tablespace: 
--

CREATE TABLE campaign_performance (
    id integer NOT NULL,
    account_id character varying NOT NULL,
    campaign_id bigint NOT NULL,
    day date NOT NULL,
    network character varying NOT NULL,
    clicks bigint NOT NULL,
    impressions bigint NOT NULL,
    cost bigint NOT NULL,
    avgpos double precision NOT NULL,
    conversions bigint NOT NULL,
    observed timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: campaign_performance_id_seq; Type: SEQUENCE; Schema: public; Owner: adwords
--

CREATE SEQUENCE campaign_performance_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: campaign_performance_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: adwords
--

ALTER SEQUENCE campaign_performance_id_seq OWNED BY campaign_performance.id;


--
-- Name: campaign_structure; Type: TABLE; Schema: public; Owner: adwords; Tablespace: 
--

CREATE TABLE campaign_structure (
    id integer NOT NULL,
    account_id character varying NOT NULL,
    campaign_id bigint NOT NULL,
    name character varying NOT NULL
);


--
-- Name: campaign_structure_id_seq; Type: SEQUENCE; Schema: public; Owner: adwords
--

CREATE SEQUENCE campaign_structure_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: campaign_structure_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: adwords
--

ALTER SEQUENCE campaign_structure_id_seq OWNED BY campaign_structure.id;


--
-- Name: keyword_attributes; Type: TABLE; Schema: public; Owner: adwords; Tablespace: 
--

CREATE TABLE keyword_attributes (
    id integer NOT NULL,
    account_id character varying NOT NULL,
    campaign_id bigint NOT NULL,
    ad_group_id bigint NOT NULL,
    keyword_id bigint NOT NULL,
    bid bigint,
    quality_score bigint NOT NULL,
    first_page_bid bigint NOT NULL,
    top_of_page_bid bigint NOT NULL,
    observed timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: keyword_attributes_id_seq; Type: SEQUENCE; Schema: public; Owner: adwords
--

CREATE SEQUENCE keyword_attributes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: keyword_attributes_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: adwords
--

ALTER SEQUENCE keyword_attributes_id_seq OWNED BY keyword_attributes.id;


--
-- Name: keyword_structure; Type: TABLE; Schema: public; Owner: adwords; Tablespace: 
--

CREATE TABLE keyword_structure (
    id integer NOT NULL,
    account_id character varying NOT NULL,
    campaign_id bigint NOT NULL,
    ad_group_id bigint NOT NULL,
    keyword_id bigint NOT NULL,
    keyword character varying NOT NULL,
    match_type character varying NOT NULL
);


--
-- Name: keyword_structure_id_seq; Type: SEQUENCE; Schema: public; Owner: adwords
--

CREATE SEQUENCE keyword_structure_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: keyword_structure_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: adwords
--

ALTER SEQUENCE keyword_structure_id_seq OWNED BY keyword_structure.id;


--
-- Name: text_ad_attributes; Type: TABLE; Schema: public; Owner: adwords; Tablespace: 
--

CREATE TABLE text_ad_attributes (
    id integer NOT NULL,
    account_id character varying NOT NULL,
    campaign_id bigint NOT NULL,
    ad_group_id bigint NOT NULL,
    ad_id bigint NOT NULL,
    status character varying NOT NULL,
    observed timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: text_ad_attributes_id_seq; Type: SEQUENCE; Schema: public; Owner: adwords
--

CREATE SEQUENCE text_ad_attributes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE text_ad_attributes_id_seq OWNER TO adwords;

--
-- Name: text_ad_attributes_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: adwords
--

ALTER SEQUENCE text_ad_attributes_id_seq OWNED BY text_ad_attributes.id;


--
-- Name: text_ad_performance; Type: TABLE; Schema: public; Owner: adwords; Tablespace: 
--

CREATE TABLE text_ad_performance (
    id integer NOT NULL,
    account_id character varying NOT NULL,
    campaign_id bigint NOT NULL,
    ad_group_id bigint NOT NULL,
    ad_id bigint NOT NULL,
    keyword_id bigint NOT NULL,
    day date NOT NULL,
    network character varying NOT NULL,
    clicks bigint NOT NULL,
    impressions bigint NOT NULL,
    cost bigint NOT NULL,
    avgpos double precision NOT NULL,
    conversions bigint NOT NULL,
    observed timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: text_ad_performance_id_seq; Type: SEQUENCE; Schema: public; Owner: adwords
--

CREATE SEQUENCE text_ad_performance_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: text_ad_performance_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: adwords
--

ALTER SEQUENCE text_ad_performance_id_seq OWNED BY text_ad_performance.id;


--
-- Name: text_ad_structure; Type: TABLE; Schema: public; Owner: adwords; Tablespace: 
--

CREATE TABLE text_ad_structure (
    id integer NOT NULL,
    account_id character varying NOT NULL,
    campaign_id bigint NOT NULL,
    ad_group_id bigint NOT NULL,
    ad_id bigint NOT NULL,
    headline character varying NOT NULL,
    description1 character varying NOT NULL,
    description2 character varying NOT NULL,
    display_url character varying NOT NULL,
    destination_url character varying NOT NULL
);


--
-- Name: text_ad_structure_id_seq; Type: SEQUENCE; Schema: public; Owner: adwords
--

CREATE SEQUENCE text_ad_structure_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: text_ad_structure_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: adwords
--

ALTER SEQUENCE text_ad_structure_id_seq OWNED BY text_ad_structure.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: adwords
--

ALTER TABLE ONLY ad_group_attributes ALTER COLUMN id SET DEFAULT nextval('ad_group_attributes_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: adwords
--

ALTER TABLE ONLY ad_group_performance ALTER COLUMN id SET DEFAULT nextval('ad_group_performance_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: adwords
--

ALTER TABLE ONLY ad_group_structure ALTER COLUMN id SET DEFAULT nextval('ad_group_structure_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: adwords
--

ALTER TABLE ONLY campaign_attributes ALTER COLUMN id SET DEFAULT nextval('campaign_attributes_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: adwords
--

ALTER TABLE ONLY campaign_performance ALTER COLUMN id SET DEFAULT nextval('campaign_performance_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: adwords
--

ALTER TABLE ONLY campaign_structure ALTER COLUMN id SET DEFAULT nextval('campaign_structure_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: adwords
--

ALTER TABLE ONLY keyword_attributes ALTER COLUMN id SET DEFAULT nextval('keyword_attributes_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: adwords
--

ALTER TABLE ONLY keyword_structure ALTER COLUMN id SET DEFAULT nextval('keyword_structure_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: adwords
--

ALTER TABLE ONLY text_ad_attributes ALTER COLUMN id SET DEFAULT nextval('text_ad_attributes_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: adwords
--

ALTER TABLE ONLY text_ad_performance ALTER COLUMN id SET DEFAULT nextval('text_ad_performance_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: adwords
--

ALTER TABLE ONLY text_ad_structure ALTER COLUMN id SET DEFAULT nextval('text_ad_structure_id_seq'::regclass);


--
-- Name: ad_group_attributes_pkey; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY ad_group_attributes
    ADD CONSTRAINT ad_group_attributes_pkey PRIMARY KEY (id);


--
-- Name: ad_group_performance_pkey; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY ad_group_performance
    ADD CONSTRAINT ad_group_performance_pkey PRIMARY KEY (id);


--
-- Name: ad_group_structure_pkey; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY ad_group_structure
    ADD CONSTRAINT ad_group_structure_pkey PRIMARY KEY (id);


--
-- Name: campaign_attributes_pkey; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY campaign_attributes
    ADD CONSTRAINT campaign_attributes_pkey PRIMARY KEY (id);


--
-- Name: campaign_performance_pkey; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY campaign_performance
    ADD CONSTRAINT campaign_performance_pkey PRIMARY KEY (id);


--
-- Name: campaign_structure_pkey; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY campaign_structure
    ADD CONSTRAINT campaign_structure_pkey PRIMARY KEY (id);


--
-- Name: keyword_attributes_pkey; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY keyword_attributes
    ADD CONSTRAINT keyword_attributes_pkey PRIMARY KEY (id);


--
-- Name: keyword_structure_pkey; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY keyword_structure
    ADD CONSTRAINT keyword_structure_pkey PRIMARY KEY (id);


--
-- Name: text_ad_attributes_pkey; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY text_ad_attributes
    ADD CONSTRAINT text_ad_attributes_pkey PRIMARY KEY (id);


--
-- Name: text_ad_performance_pkey; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY text_ad_performance
    ADD CONSTRAINT text_ad_performance_pkey PRIMARY KEY (id);


--
-- Name: text_ad_structure_pkey; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY text_ad_structure
    ADD CONSTRAINT text_ad_structure_pkey PRIMARY KEY (id);


--
-- Name: unique_ad_group_structure; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY ad_group_structure
    ADD CONSTRAINT unique_ad_group_structure UNIQUE (ad_group_id);


--
-- Name: unique_campaign_structure; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY campaign_structure
    ADD CONSTRAINT unique_campaign_structure UNIQUE (campaign_id);


--
-- Name: unique_keyword_structure; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY keyword_structure
    ADD CONSTRAINT unique_keyword_structure UNIQUE (ad_group_id, keyword_id);


--
-- Name: unique_text_ad_structure; Type: CONSTRAINT; Schema: public; Owner: adwords; Tablespace: 
--

ALTER TABLE ONLY text_ad_structure
    ADD CONSTRAINT unique_text_ad_structure UNIQUE (ad_group_id, ad_id);


-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: public; Owner: postgres
--

CREATE TABLE database_schema (
       id SERIAL PRIMARY KEY,
       description TEXT NOT NULL,
       file TEXT NOT NULL,
       timestamp TIMESTAMP DEFAULT now()
       );
INSERT INTO database_schema(description,file) VALUES ('Baseline schema','0000-baseline.sql');
