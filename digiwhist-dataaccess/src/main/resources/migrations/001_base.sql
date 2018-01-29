SET search_path TO digiwhist_perf;

CREATE TABLE crawler_audit_record (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX crawler_audit_record_modifiedby_idx ON crawler_audit_record (modifiedby);
CREATE INDEX crawler_audit_record_modifiedbyversion_idx ON crawler_audit_record (modifiedbyversion);


CREATE TABLE raw_data (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX raw_data_createdby_idx ON raw_data (createdby);
CREATE INDEX raw_data_modified_idx ON raw_data (modified);
CREATE INDEX raw_data_modifiedby_idx ON raw_data (modifiedby);
CREATE INDEX raw_data_modifiedbyversion_idx ON raw_data (modifiedbyversion);
CREATE INDEX raw_dada_persistentid_idx ON raw_data (createdby, (data->>'persistentId'));


CREATE TABLE parsed_tender (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX parsed_tender_data_idx ON parsed_tender USING gin (data jsonb_path_ops);
CREATE INDEX parsed_tender_createdby_idx ON parsed_tender (createdby);
CREATE INDEX parsed_tender_modified_idx ON parsed_tender (modified);
CREATE INDEX parsed_tender_modifiedby_idx ON parsed_tender (modifiedby);
CREATE INDEX parsed_tender_modifiedbyversion_idx ON parsed_tender (modifiedbyversion);


CREATE TABLE clean_tender (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX clean_tender_data_idx ON clean_tender USING gin (data jsonb_path_ops);
CREATE INDEX clean_tender_createdby_idx ON clean_tender (createdby);
CREATE INDEX clean_tender_modified_idx ON clean_tender (modified);
CREATE INDEX clean_tender_modifiedby_idx ON clean_tender (modifiedby);
CREATE INDEX clean_tender_modifiedbyversion_idx ON clean_tender (modifiedbyversion);
CREATE INDEX clean_tender_modified_country_idx ON clean_tender (modified, (data->>'country'));
ALTER TABLE clean_tender SET (autovacuum_vacuum_scale_factor = 0.0);
ALTER TABLE clean_tender SET (autovacuum_vacuum_threshold = 10000);
ALTER TABLE clean_tender SET (autovacuum_analyze_scale_factor = 0.0);
ALTER TABLE clean_tender SET (autovacuum_analyze_threshold = 10000);


CREATE TABLE matched_tender (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX matched_tender_data_idx ON matched_tender USING gin (data jsonb_path_ops);
CREATE INDEX matched_tender_createdby_idx ON matched_tender (createdby);
CREATE INDEX matched_tender_modified_idx ON matched_tender (modified);
CREATE INDEX matched_tender_modifiedby_idx ON matched_tender (modifiedby);
CREATE INDEX matched_tender_modifiedbyversion_idx ON matched_tender (modifiedbyversion);
ALTER TABLE matched_tender SET (autovacuum_vacuum_scale_factor = 0.0);
ALTER TABLE matched_tender SET (autovacuum_vacuum_threshold = 10000);
ALTER TABLE matched_tender SET (autovacuum_analyze_scale_factor = 0.0);
ALTER TABLE matched_tender SET (autovacuum_analyze_threshold = 10000);


CREATE TABLE matched_body (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX matched_body_data_idx ON matched_body USING gin (data jsonb_path_ops);
CREATE INDEX matched_body_createdby_idx ON matched_body (createdby);
CREATE INDEX matched_body_modified_idx ON matched_body (modified);
CREATE INDEX matched_body_modifiedby_idx ON matched_body (modifiedby);
CREATE INDEX matched_body_modifiedbyversion_idx ON matched_body (modifiedbyversion);
CREATE INDEX matched_body_digest_idx ON matched_body ((data ->> 'digest'::text));
CREATE INDEX matched_body_std_name_idx ON matched_body (md5(data ->> 'standardizedName'::text));
CREATE INDEX matched_body_std_address_idx ON matched_body (md5(data ->> 'standardizedAddress'::text));
ALTER TABLE matched_body SET (autovacuum_vacuum_scale_factor = 0.0);
ALTER TABLE matched_body SET (autovacuum_vacuum_threshold = 20000);
ALTER TABLE matched_body SET (autovacuum_analyze_scale_factor = 0.0);
ALTER TABLE matched_body SET (autovacuum_analyze_threshold = 20000);


CREATE TABLE master_tender (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX master_tender_data_idx ON master_tender USING gin (data jsonb_path_ops);
CREATE INDEX master_tender_createdby_idx ON master_tender (createdby);
CREATE INDEX master_tender_modified_idx ON master_tender (modified);
CREATE INDEX master_tender_modifiedby_idx ON master_tender (modifiedby);
CREATE INDEX master_tender_modifiedbyversion_idx ON master_tender (modifiedbyversion);
CREATE INDEX master_tender_modified_country_idx ON master_tender (modified, (data->>'country'));


CREATE TABLE master_body (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX master_body_data_idx ON master_body USING gin (data jsonb_path_ops);
CREATE INDEX master_body_createdby_idx ON master_body (createdby);
CREATE INDEX master_body_modified_idx ON master_body (modified);
CREATE INDEX master_body_modifiedby_idx ON master_body (modifiedby);
CREATE INDEX master_body_modifiedbyversion_idx ON master_body (modifiedbyversion);


CREATE TABLE manual_match (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255),
    flag character varying(255),
 	groupid character varying(255),
 	hash character varying(255)
);

CREATE INDEX manual_match_flag_idx ON manual_match (flag);
CREATE INDEX manual_match_hash_idx ON manual_match (hash);


CREATE TABLE plain_document (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX plain_document_data_idx ON plain_document USING gin (data jsonb_path_ops);
CREATE INDEX plain_document_modifiedby_idx ON plain_document (modifiedby);
CREATE INDEX plain_document_modifiedbyversion_idx ON plain_document (modifiedbyversion);


CREATE TABLE parsed_budget_item (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX parsed_budget_item_data_idx ON parsed_budget_item USING gin (data jsonb_path_ops);
CREATE INDEX parsed_budget_item_modifiedby_idx ON parsed_budget_item (modifiedby);
CREATE INDEX parsed_budget_item_modifiedbyversion_idx ON parsed_budget_item (modifiedbyversion);


CREATE TABLE clean_budget_item (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX clean_budget_item_data_idx ON clean_budget_item USING gin (data jsonb_path_ops);
CREATE INDEX clean_budget_item_modifiedby_idx ON clean_budget_item (modifiedby);
CREATE INDEX clean_budget_item_modifiedbyversion_idx ON clean_budget_item (modifiedbyversion);


CREATE TABLE indicator (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX indicator_data_idx ON indicator USING gin (data jsonb_path_ops);


CREATE TABLE parsed_public_official (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255),
    full_name text
);

CREATE INDEX parsed_public_official_modifiedby_idx ON parsed_public_official (modifiedby);
CREATE INDEX parsed_public_official_modifiedbyversion_idx ON parsed_public_official (modifiedbyversion);
CREATE INDEX parsed_public_official_full_name_idx ON parsed_public_official (full_name);


CREATE TABLE exchange_rates (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX exchange_rates_data_idx ON exchange_rates USING gin (data jsonb_path_ops);




DROP TABLE if exists clean_tender_analytics cascade;
CREATE TABLE clean_tender_analytics
(
  tender_id text,
  title text,
  procedure_type text,
  national_procedure_type text,
  is_awarded boolean,
  supply_type text,
  size text,
  cpvs text,
  cpv_category text,
  bid_deadline date,
  is_central_procurement boolean,
  is_joint_procurement boolean,
  is_framework_agreement boolean,
  is_dps boolean,
  is_on_behalf_of boolean,
  on_behalf_of_body_count integer,
  lots_count integer,
  recorded_bids_count integer,
  documents_count integer,
  npwp_reasons text,
  estimated_duration_in_days integer,
  contract_signature_date date,
  is_eu_funded boolean,
  selection_method text,
  award_criteria_count integer,
  price_criteria_weight float,
  is_electronic_auction boolean,
  cancellation_date date,
  cancellation_reason text,
  award_deadline date,
  tender_estimated_price double precision,
  lot_estimated_price double precision,
  lot_estimated_price_eur double precision,
  tender_final_price double precision,
  bid_final_price double precision,
  bid_final_price_eur double precision,
  corrections_count integer,
  lot_number integer,
  lot_title text,
  status text,
  bids_count integer,
  valid_bids_count integer,
  electronic_bids_count integer,
  sme_bids_count integer,
  other_eu_member_states_companies_bids_count integer,
  non_eu_member_states_companies_bids_count integer,
  foreign_companies_bids_count integer,
  buyer_id text,
  buyer_name text,
  buyer_nuts text,
  buyer_email text,
  buyer_contact_name text,
  buyer_contact_point text,
  buyer_country text,
  buyer_main_activity text,
  buyer_type text,
  bidder_id text,
  bidder_name text,
  bidder_nuts text,
  bidder_country text,
  is_subcontracted boolean,
  subcontracted_proportion double precision,
  is_consortium boolean,
  source text,
  form_type text,
  source_form_type text,
  form_type_combined text,
  source_tender_id text,
  source_publication text,
  source_publication_date date,
  year integer,
  award_count integer,
  notice_count integer,
  notice_date_first date,
  notice_date_last date,
  award_date date,
  notice_url text,
  award_url text,
  created timestamp without time zone,
  createdby text,
  createdbyversion double precision,
  modified timestamp without time zone,
  modifiedby text,
  persistent_id text,
  analytics_created_time timestamp without time zone,
  main_cpv_first boolean,
  is_winning boolean,
  tender_id_vestnik text
)
WITH (
  OIDS=FALSE
);

CREATE INDEX analytics_cta_form_type_combined_idx
  ON clean_tender_analytics
  USING btree
  (form_type_combined COLLATE pg_catalog."default");

CREATE INDEX analytics_cta_createdby_idx
  ON clean_tender_analytics
  USING btree
  (createdby COLLATE pg_catalog."default");

CREATE INDEX analytics_cta_persistent_id_idx
  ON clean_tender_analytics
  USING btree
  (persistent_id COLLATE pg_catalog."default");

CREATE INDEX analytics_cta_source_form_idx
  ON clean_tender_analytics
  USING btree
  (source_form_type COLLATE pg_catalog."default");

CREATE INDEX analytics_cta_source_idx
  ON clean_tender_analytics
  USING btree
  (source COLLATE pg_catalog."default");

CREATE INDEX analytics_cta_source_publication_idx
  ON clean_tender_analytics
  USING btree
  (source_publication COLLATE pg_catalog."default");

CREATE INDEX analytics_cta_tender_id_idx
  ON clean_tender_analytics
  USING btree
  (tender_id COLLATE pg_catalog."default");

CREATE INDEX analytics_cta_tender_proceduretype_idx
  ON clean_tender_analytics
  USING btree
  (procedure_type COLLATE pg_catalog."default");

CREATE INDEX analytics_cta_tender_year_idx
  ON clean_tender_analytics
  USING btree
  (year);

  CREATE MATERIALIZED VIEW digiwhist_validation.conflicting_persistent_ids AS 
 SELECT clean_tender_analytics.persistent_id,
    clean_tender_analytics.source,
    count(DISTINCT clean_tender_analytics.tender_id) AS tender_cnt
   FROM digiwhist_staging.clean_tender_analytics
  GROUP BY clean_tender_analytics.persistent_id, clean_tender_analytics.source
 HAVING count(DISTINCT clean_tender_analytics.tender_id) > 1
WITH DATA;

  
  -- Table: matched_tender_analytics
DROP TABLE if exists matched_tender_analytics;
CREATE TABLE matched_tender_analytics
(
  matched_tender_id text,
  tender_group_id text,
  clean_tender_id text,
  persistent_id text,
  matched_by text,
  buyer_group_id text,
  bidder_group_id text,
  source text,
  publication_formtype text,
  created timestamp with time zone,
  createdby text,
  createdbyversion double precision,
  analytics_created_time timestamp with time zone
)
WITH (
  OIDS=FALSE
);


CREATE INDEX analytics_mta_bgi_idx
  ON matched_tender_analytics
  USING btree
  (buyer_group_id COLLATE pg_catalog."default");

CREATE INDEX analytics_mta_cb_idx
  ON matched_tender_analytics
  USING btree
  (createdby COLLATE pg_catalog."default");

CREATE INDEX analytics_mta_cti_idx
  ON matched_tender_analytics
  USING btree
  (clean_tender_id COLLATE pg_catalog."default");

CREATE INDEX analytics_mta_mt_idx
  ON matched_tender_analytics
  USING btree
  (createdby COLLATE pg_catalog."default");

CREATE INDEX analytics_mta_tgi_idx
  ON matched_tender_analytics
  USING btree
  (tender_group_id COLLATE pg_catalog."default");

CREATE INDEX analytics_mta_tpi_idx
  ON matched_tender_analytics
  USING btree
  (persistent_id COLLATE pg_catalog."default");


  
  
  
  -- Table: master_tender_analytics
DROP TABLE if exists master_tender_analytics;
CREATE TABLE master_tender_analytics
(
  tender_id text,
  tender_group_id text,
  title text,
  title_english text,
  procedure_type text,
  national_procedure_type text,
  is_gpa boolean,
  is_awarded boolean,
  eligible_languages text,
  supply_type text,
  cpvs text,
  main_cpv_first boolean,
  implementation_nuts text,
  size text,
  bid_deadline date,
  is_central_procurement boolean,
  is_joint_procurement boolean,
  is_framework_agreement boolean,
  is_dps boolean,
  is_on_behalf_of boolean,
  on_behalf_of_body_count integer,
  lots_count integer,
  recorded_bids_count integer,
  documents_count integer,
  npwp_reasons text,
  estimated_duration_in_days integer,
  contract_signature_date date,
  is_eu_funded boolean,
  selection_method text,
  award_criteria_count integer,
  price_criteria_weight float,
  is_electronic_auction boolean,
  cancellation_date date,
  cancellation_reason text,
  award_deadline date,
  tender_estimated_price double precision,
  lot_estimated_price double precision,
  lot_estimated_price_eur double precision,
  robust_lot_estimated_price double precision,
  robust_lot_estimated_price_eur double precision,
  robust_lot_estimated_price_reliability double precision,
  tender_final_price double precision,
  bid_final_price double precision,
  bid_final_price_eur double precision,
  robust_bid_final_price double precision,
  robust_bid_final_price_eur double precision,
  robust_bid_final_price_reliability double precision,
  corrections_count integer,
  lot_number integer,
  lot_title text,
  status text,
  bids_count integer,
  valid_bids_count integer,
  electronic_bids_count integer,
  sme_bids_count integer,
  other_eu_member_states_companies_bids_count integer,
  non_eu_member_states_companies_bids_count integer,
  foreign_companies_bids_count integer,
  buyer_id text,
  buyer_group_id text,
  buyer_master_id text,
  buyer_name text,
  buyer_postcode text,
  buyer_nuts text,
  buyer_email text,
  buyer_contact_name text,
  buyer_contact_point text,
  buyer_city text,
  buyer_country text,
  buyer_main_activity text,
  buyer_type text,
  bidder_id text,
  bidder_group_id text,
  bidder_master_id text,
  bidder_name text,
  bidder_postcode text,
  bidder_nuts text,
  bidder_city text,
  bidder_country text,
  bidder_foundation_date date,
  bidder_employees_count integer,
  is_subcontracted boolean,
  subcontracted_proportion double precision,
  is_consortium boolean,
  source text,
  included_publications text,
  included_publication_count integer,
  year integer,
  notice_count integer,
  notice_date_first date,
  notice_date_last date,
  notice_url text,
  award_count integer,
  award_date date,
  award_url text,
  created timestamp with time zone,
  createdby text,
  createdbyversion double precision,
  modified timestamp with time zone,  
  modifiedby text,  
  persistent_id text,
  analytics_created_time timestamp with time zone,
  cpv_category text,
  source_tender_id text,
  is_winning boolean,
  tender_id_vestnik text,
  description_length integer,
  lot_description_length integer,
  personal_reqs_length integer,
  technical_reqs_length integer,
  economic_reqs_length integer,
  flag_single_bid float,
  flag_centralized float,
  flag_advertisement_period float,
  flag_decision_period float,
  flag_gpa float,
  flag_eauction float,
  flag_framework float,
  flag_english_allowed float,
  flag_cft_missing float,
  flag_procedure_type float,
  flag_new_company float,
  flag_tax_haven float,
  flag_missing_fields float,
  flag_political_tie float,
  flag_publication_rate float,
  flag_desc_length float,
  flag_eligibility_length float,
  flag_evaluation_criteria float,
  opentender boolean
)
WITH (
  OIDS=FALSE
);


CREATE INDEX analytics_mta_createdby_idx
  ON master_tender_analytics
  USING btree
  (createdby COLLATE pg_catalog."default");

CREATE INDEX analytics_mta_persistent_id_idx
  ON master_tender_analytics
  USING btree
  (persistent_id COLLATE pg_catalog."default");

CREATE INDEX analytics_mta_source_idx
  ON master_tender_analytics
  USING btree
  (source COLLATE pg_catalog."default");

CREATE INDEX analytics_mta_tender_group_id_idx
  ON master_tender_analytics
  USING btree
  (tender_group_id COLLATE pg_catalog."default");

CREATE INDEX analytics_mta_tender_id_idx
  ON master_tender_analytics
  USING btree
  (tender_id COLLATE pg_catalog."default");

CREATE INDEX analytics_mta_tender_proceduretype_idx
  ON master_tender_analytics
  USING btree
  (procedure_type COLLATE pg_catalog."default");

CREATE INDEX analytics_mta_tender_year_idx
  ON master_tender_analytics
  USING btree
  (year);
  
CREATE INDEX analytics_mta_buyer_id_idx
  ON master_tender_analytics
  USING btree
  (buyer_master_id COLLATE pg_catalog."default");

  CREATE INDEX analytics_mta_bidder_id_idx
  ON master_tender_analytics
  USING btree
  (bidder_master_id COLLATE pg_catalog."default");


DROP TABLE if exists matched_body_analytics;

CREATE TABLE matched_body_analytics
(
  id text,
  body_group_id text,
  clean_tender_id text,
  clean_tender_persistent_id text,
  name text,
  standardized_name text,
  standardized_address text,
  role text,
  source text,
  digest text,
  hash text,
  matched_by text,
  completeness_score double precision,
  matching_score double precision,
  organisation_id text,
  some_id text,
  createdby character varying(255),
  created timestamp with time zone,
  createdbyversion double precision,
  analytics_created_time timestamp with time zone
)
WITH (
  OIDS=FALSE
);

CREATE INDEX analytics_mba_bgi_idx
  ON matched_body_analytics
  USING btree
  (body_group_id COLLATE pg_catalog."default");

CREATE INDEX analytics_mba_cti_idx
  ON matched_body_analytics
  USING btree
  (clean_tender_id COLLATE pg_catalog."default");

CREATE INDEX analytics_mba_ctpi_idx
  ON matched_body_analytics
  USING btree
  (clean_tender_persistent_id COLLATE pg_catalog."default");

CREATE INDEX analytics_mba_id_idx
  ON matched_body_analytics
  USING btree
  (id COLLATE pg_catalog."default");

CREATE INDEX analytics_mba_cby_idx
  ON matched_body_analytics
  USING btree
  (createdby COLLATE pg_catalog."default");

CREATE INDEX analytics_mba_sn_trgm_idx
    ON matched_body_analytics
    USING GIST
    (standardized_name gist_trgm_ops);



-- Table: master_body_analytics
DROP TABLE if exists master_body_analytics;
CREATE TABLE master_body_analytics
(
  id character varying(255),
  group_id text,
  name text,
  street text,
  city text,
  nuts text,
  postcode text,
  country text,
  raw_address text,
  contact_name text,
  contact_point text,
  main_activity text,
  buyer_type text,
  organisation_id text,
  tax_id text,
  trade_register_id text,
  etalon_id text,
  some_id text,
  employees_count integer,
  foundation_date date,
  created timestamp with time zone,
  createdby character varying(255),
  createdbyversion double precision,
  analytics_created_time timestamp with time zone,
  email text
)
WITH (
  OIDS=FALSE
);
  
  
CREATE INDEX analytics_mba_createdby_idx
  ON master_body_analytics
  USING btree
  (createdby COLLATE pg_catalog."default");
