SET search_path TO bvd;

CREATE TABLE registry_information (
    bvd_id_number character varying(1024),
    name character varying(1024),
    also_known_as_name character varying(1024),
    street_no_building_etc_line_1 character varying(1024),
    street_no_building_etc_line_2 character varying(1024),
    street_no_building_etc_line_3 character varying(1024),
    street_no_building_etc_line_4 character varying(1024),
    postcode character varying(1024),
    city character varying(1024),
    country character varying(1024),
    country_iso_code character varying(1024),
    nuts3 character varying(1024),
    european_vat_number character varying(1024),
    vattax_number character varying(1024),
    trade_register_number character varying(1024),
    statistical_number character varying(1024),
    other_company_id_number character varying(1024),
    national_id_number character varying(1024),
    national_id_label character varying(1024),
    national_id_type character varying(1024),
    national_legal_form character varying(1024),
    status character varying(1024),
    status_date character varying(1024),
    status_updated_in_orbis character varying(1024),
    date_of_incorporation character varying(1024),
    reporting_basis character varying(1024),
    nace_rev_2_core_code_4_digits character varying(1024),
    nace_rev_2_core_code__text_description character varying(1024),
    listeddelistedunlisted character varying(1024),
    category_of_the_company character varying(1024),
    number_of_employees_last_value character varying(1024),
    main_foreign_countries_or_regions character varying(5096),
    main_production_sites character varying(5096),
    main_distribution_sites character varying(5096),
    standardizedaddress character varying(1024),
    standardizedname character varying(4096),
    digest character varying(255),
    digest2 character varying(255),
    id integer NOT NULL
);


CREATE INDEX registry_information_country_iso_code_idx ON registry_information USING btree (country_iso_code);
CREATE INDEX registry_information_digest_idx ON registry_information USING btree (digest);
CREATE INDEX registry_information_standardizedaddress_idx ON registry_information USING btree (standardizedaddress);
CREATE INDEX registry_information_standardizedname_idx ON registry_information USING btree (standardizedname);
CREATE INDEX registry_information_statistical_number_idx ON registry_information USING btree (statistical_number);
CREATE INDEX registry_information_trade_register_number_idx ON registry_information USING btree (trade_register_number);
CREATE INDEX registry_information_vattax_number_idx ON registry_information USING btree (vattax_number);
CREATE INDEX registry_information_digest2_idx ON registry_information USING btree (digest2);

CREATE SEQUENCE bvd.links_seq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 1
  CACHE 1;

CREATE TABLE links (
    subsidiary_bvd_id character varying(32),
    subsidiary_independence_indicator character varying(32),
    shareholder_bvd_id character varying(32),
    shareholder_independence_indicator character varying(32),
    direct_percent character varying(32),
    direct_percent_only_figures character varying(32),
    total_percent character varying(32),
    total_percent_only_figures character varying(32),
    information_date character varying(32),
    source character varying(32),
    type_of_relation character varying(32),
    active_archived_ratio character varying(32),
    guo_25 character varying(32),
    guo_50 character varying(32),
    guo_50c character varying(32),
    guo_25c character varying(32),
    id integer NOT NULL DEFAULT nextval('bvd.links_seq'::regclass),
    CONSTRAINT links_idx PRIMARY KEY (id)
);

CREATE SEQUENCE manager_information_seq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 1
  CACHE 1;

CREATE TABLE manager_information (
    bvd_id_number character varying(32),
    number_of_dmc character varying(32),
    number_of_dmc_current character varying(32),
    number_of_dmc_previous character varying(32),
    full_name character varying(1000),
    uci character varying(32),
    original_job_title_en character varying(1000),
    appointment_date character varying(32),
    resignation_date character varying(32),
    gender character varying(32),
    date_of_birth character varying(32),
    countries_of_nationality character varying(32),
    country character varying(32),
    is_shareholder character varying(32),
    id integer NOT NULL DEFAULT nextval('bvd.manager_information_seq'::regclass),
    CONSTRAINT manager_information_idx PRIMARY KEY (id)
);

CREATE SEQUENCE entities_seq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 1
  CACHE 1;

CREATE TABLE entities (
    bvd_id character varying(32),
    name character varying(1000),
    country character varying(3),
    entity_type character varying(3),
    id integer NOT NULL DEFAULT nextval('bvd.entities_seq'::regclass),
    CONSTRAINT entities_idx PRIMARY KEY (id)
);

CREATE SEQUENCE financial_information_seq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 1
  CACHE 1;

CREATE TABLE financial_information (
    bvd_id character varying(32),
    consolidation_code character varying(32),
    filing_type character varying(32),
    closing_date character varying(10),
    original_units character varying(10),
    original_currency character varying(5),
    exchange_rate character varying(10),
    operating_revenue_turnover character varying(32),
    financial_revenue character varying(32),
    extr_other_revenue character varying(32),
    material_costs character varying(32),
    costs_of_employees character varying(32),
    gross_profit character varying(32),
    depreciation_and_amortization character varying(32),
    research_and_development_expenses character varying(32),
    financial_expenses character varying(32),
    financial_p_l character varying(32),
    p_l_before_tax character varying(32),
    taxation character varying(32),
    p_l_after_tax character varying(32),
    ebitda character varying(32),
    roa_using_p_l_before_tax_percentage character varying(32),
    tangible_fixed_assets character varying(32),
    intangible_fixed_assets character varying(32),
    total_assets character varying(32),
    total_shareh_funds_and_liab character varying(32),
    non_current_liabilities character varying(32),
    current_liabilities character varying(32),
    id integer NOT NULL DEFAULT nextval('bvd.financial_information_seq'::regclass),
    CONSTRAINT financial_information_idx PRIMARY KEY (id)
);