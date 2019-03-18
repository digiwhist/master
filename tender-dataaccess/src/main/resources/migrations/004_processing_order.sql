SET search_path TO tender_development;

CREATE INDEX raw_data_processingorder_idx ON raw_data ((data->>'processingOrder'));
CREATE INDEX parsed_tender_processingorder_idx ON raw_data ((data->>'processingOrder'));
CREATE INDEX clean_tender_processingorder_idx ON raw_data ((data->>'processingOrder'));
CREATE INDEX matched_tender_processingorder_idx ON raw_data ((data->>'processingOrder'));
CREATE INDEX matched_body_processingorder_idx ON raw_data ((data->>'processingOrder'));
CREATE INDEX master_tender_processingorder_idx ON master_tender ((data->>'processingOrder'));
CREATE INDEX master_body_processingorder_idx ON master_body ((data->>'processingOrder'));


CREATE INDEX raw_data_createdby_processingorder_idx ON raw_data (createdby, (data->>'processingOrder'));
CREATE INDEX parsed_tender_createdby_processingorder_idx ON raw_data (createdby, (data->>'processingOrder'));
CREATE INDEX clean_tender_createdby_processingorder_idx ON raw_data (createdby, (data->>'processingOrder'));
CREATE INDEX matched_tender_createdby_processingorder_idx ON raw_data (createdby, (data->>'processingOrder'));
CREATE INDEX matched_body_createdby_processingorder_idx ON raw_data (createdby, (data->>'processingOrder'));
CREATE INDEX master_tender_createdby_processingorder_idx ON master_tender (createdby, (data->>'processingOrder'));
CREATE INDEX master_body_createdby_processingorder_idx ON master_body (createdby, (data->>'processingOrder'));