SET
    search_path TO tender_development;

ALTER TABLE
    master_tender_analytics
ADD
    src text NULL;

CREATE INDEX analytics_mta_src_idx ON master_tender_analytics (src);

ALTER TABLE
    matched_tender_analytics
ADD
    src text NULL;

CREATE INDEX analytics_mta_mtq_idx ON matched_tender_analytics (src);

ALTER TABLE
    clean_tender_analytics
ADD
    src text NULL;

CREATE INDEX analytics_cta_src_idx ON clean_tender_analytics (src);