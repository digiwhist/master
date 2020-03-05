ALTER TABLE tender_production.master_tender_analytics ADD src text NULL;
CREATE INDEX analytics_mta_src_idx ON tender_production.master_tender_analytics (src);

ALTER TABLE tender_production.matched_tender_analytics ADD src text NULL;
CREATE INDEX analytics_mta_mtq_idx ON tender_production.matched_tender_analytics (src);

ALTER TABLE tender_production.clean_tender_analytics ADD src text NULL;
CREATE INDEX analytics_cta_src_idx ON tender_production.clean_tender_analytics (src);