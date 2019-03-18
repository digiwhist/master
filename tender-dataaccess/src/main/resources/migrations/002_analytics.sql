SET search_path TO tender_development;

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
  tender_id_vestnik text,
  award_decision_date date,
  lot_array_index integer,
  bid_array_index integer,
  amendments_count integer,
  amendment_date_last date,
  updated_price double precision,
  updated_completion_date date,
  updated_duration_days integer,
  payments_sum double precision,
  last_payment_year integer
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
  buyer_bvd_id text,
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
  bidder_bvd_id text,
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
  opentender boolean,
  tender_country text,
  tender_digiwhist_price double precision,
  bid_digiwhist_price double precision,
  currency text,
  award_decision_date date,
  lot_array_index integer,
  bid_array_index integer,
  amendments_count integer,
  updated_price double precision,
  updated_completion_date date,
  updated_duration_days integer,
  payments_sum double precision,
  last_payment_year integer
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
  
    CREATE INDEX analytics_mta_bidder_bvd_id_idx
  ON master_tender_analytics
  USING btree
  (bidder_bvd_id COLLATE pg_catalog."default");

  CREATE INDEX analytics_mta_tender_country_idx
  ON master_tender_analytics
  USING btree
  (tender_country COLLATE pg_catalog."default");


DROP TABLE if exists matched_body_analytics;

CREATE TABLE matched_body_analytics
(
  id text,
  group_id text,
  clean_tender_id text,
  clean_tender_persistent_id text,
  name text,
  standardized_name text,
  standardized_address text,
  role text,
  source text,
  digest text,
  hash text,
  full_hash text,
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
  (group_id COLLATE pg_catalog."default");

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
  bvd_id text,
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