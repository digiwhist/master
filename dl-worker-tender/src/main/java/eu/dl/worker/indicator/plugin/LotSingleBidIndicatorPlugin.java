package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

import java.util.Arrays;
import java.util.HashMap;

import static eu.dl.dataaccess.dto.codetables.TenderLotStatus.AWARDED;
import static eu.dl.dataaccess.dto.codetables.TenderLotStatus.FINISHED;

/**
 * This plugin calculates corruption single bid indicator.
 */
public class LotSingleBidIndicatorPlugin extends LotIndicatorPlugin {

    @Override
    public final Indicator evaluate(final MasterTenderLot lot, final MasterTender tender) {
        if (!Arrays.asList(AWARDED, FINISHED).contains(lot.getStatus())) {
            return undefined();
        }

        HashMap<String, Object> metaData = new HashMap<>();
        metaData.put("lotTitle", lot.getTitle());

        if (lot.getValidBidsCount() == null && lot.getBidsCount() == null) {
            return insufficient(metaData);
        } else {
            double value = ((lot.getValidBidsCount() != null && lot.getValidBidsCount() > 1)
                || (lot.getValidBidsCount() == null && lot.getBidsCount() > 1)) ? 100d : 0d;

            return calculated(value, metaData);
        }
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.INTEGRITY_SINGLE_BID.name();
    }
}
