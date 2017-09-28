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
