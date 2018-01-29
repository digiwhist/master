package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.utils.TenderUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * This plugin calculates Single bid indicator.
 */
public class SingleBidIndicatorPlugin extends BaseIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null) {
            throw new IllegalArgumentException("Unable to calculate indicator for null tender.");
        }

        // no contract award
        if (!TenderUtils.hasContractAward(tender)) {
            return undefined();
        }

        if (tender.getLots() == null || tender.getLots().isEmpty()) {
            return insufficient();
        }

        List<String> lotTitles = new ArrayList<String>();

        Boolean allValuesNull = true;

        // iterate over lots and try to identify those, with only single bid
        for (MasterTenderLot lot : tender.getLots()) {
            if (lot.getValidBidsCount() != null) {
                allValuesNull = false;

                if (lot.getValidBidsCount() == 1) {
                    lotTitles.add(lot.getTitle());
                }
            } else if (lot.getBidsCount() != null) {
                allValuesNull = false;

                if (lot.getBidsCount() == 1) {
                    lotTitles.add(lot.getTitle());
                }
            }
        }


        if (allValuesNull) {
            // there are no values in bids count, the data to calculate
            // indicator are insufficient
            return insufficient();
        } else {
            if (!lotTitles.isEmpty()) {
                // at least one lot has only one bid
                // store to metadata lotIds with the single bid
                HashMap<String, Object> metaData = new HashMap<String, Object>();
                metaData.put("lotTitles", lotTitles);

                return calculated(0d, metaData);
            }

            // there is no lot with a single bid
            return calculated(100d);
        }
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.INTEGRITY_SINGLE_BID.name();
    }

}
