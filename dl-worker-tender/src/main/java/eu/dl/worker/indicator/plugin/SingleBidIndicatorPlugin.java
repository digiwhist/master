package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.BasicEntityRelatedIndicator;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * This plugin calculates Single bid indicator.
 */
public class SingleBidIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaulate(final MasterTender tender) {
        if (tender == null || tender.getLots() == null) {
            return null;
        }

        Indicator indicator = new BasicEntityRelatedIndicator();
        indicator.setType(getType());
        List<String> lotTitles = new ArrayList<String>();

        // iterate over lots and try to identify those, which only single bid
        for (MasterTenderLot lot : tender.getLots()) {
            if (lot.getValidBidsCount() != null) {
                if (lot.getValidBidsCount() == 1) {
                    lotTitles.add(lot.getTitle());
                }
            } else if (lot.getBidsCount() != null && lot.getBidsCount() == 1) {
                lotTitles.add(lot.getTitle());
            }
        }

        if (!lotTitles.isEmpty()) {
            // at least one lot has only one bid
            
            // store to metadata lotIds with the single bid
            HashMap<String, Object> metaData = new HashMap<String, Object>();
            metaData.put("lotTitles", lotTitles);
            indicator.setMetaData(metaData);

            return indicator;
        } else {
            return null;
        }
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.CORRUPTION_SINGLE_BID.name();
    }

}
