SET search_path TO tender_production;

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
CREATE INDEX raw_dada_persistentid_idx ON raw_data ((data->>'persistentId'));


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
CREATE INDEX clean_budget_item_level3code_idx ON clean_budget_item ((data->>'level3Code'));
CREATE INDEX clean_budget_item_ico_idx ON clean_budget_item ((data#>>'{body,bodyIds,0,id}'));


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


CREATE TABLE postcode_nuts (
    postcode character varying(50),
    nuts character varying(10),
    country character varying(2)
);
CREATE INDEX postcode_nuts_postcode_idx ON postcode_nuts (postcode);
CREATE INDEX postcode_nuts_country_idx ON postcode_nuts (country);




