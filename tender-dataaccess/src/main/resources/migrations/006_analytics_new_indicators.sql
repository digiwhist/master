SET search_path TO tender_development;

ALTER TABLE master_tender_analytics ADD flag_award_date_missing float NULL;
ALTER TABLE master_tender_analytics ADD flag_market_missing float NULL;
ALTER TABLE master_tender_analytics ADD flag_proc_method_missing float NULL;
ALTER TABLE master_tender_analytics ADD flag_title_missing float NULL;
ALTER TABLE master_tender_analytics ADD flag_year_missing float NULL;
ALTER TABLE master_tender_analytics ADD flag_buyer_name_missing float NULL;
ALTER TABLE master_tender_analytics ADD flag_buyer_loc_missing float NULL;
ALTER TABLE master_tender_analytics ADD flag_bidder_id_missing float NULL;
ALTER TABLE master_tender_analytics ADD flag_bidder_name_missing float NULL;
ALTER TABLE master_tender_analytics ADD flag_value_missing float NULL;
ALTER TABLE master_tender_analytics ADD flag_cost_overrun float NULL;
