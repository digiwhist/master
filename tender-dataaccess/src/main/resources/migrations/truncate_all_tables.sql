SET search_path TO tender_development;

TRUNCATE TABLE crawler_audit_record;

TRUNCATE TABLE raw_data;

TRUNCATE TABLE clean_tender;

TRUNCATE TABLE parsed_tender;

TRUNCATE TABLE parsed_public_official;

TRUNCATE TABLE matched_tender;

TRUNCATE TABLE matched_body;

TRUNCATE TABLE master_tender;

TRUNCATE TABLE master_body;

TRUNCATE TABLE manual_match;

TRUNCATE TABLE plain_document;

TRUNCATE TABLE indicator;
