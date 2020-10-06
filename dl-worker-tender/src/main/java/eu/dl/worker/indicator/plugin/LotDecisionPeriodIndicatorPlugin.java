package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

import java.time.temporal.ChronoUnit;
import java.util.HashMap;

/**
 * This plugin calculates corruption decision period indicator.
 */
public class LotDecisionPeriodIndicatorPlugin extends LotIndicatorPlugin {
    @Override
    public final Indicator evaluate(final MasterTenderLot lot, final MasterTender tender) {
        if (tender == null || tender.getCountry() == null) {
            return insufficient();
        }

        if (tender.getBidDeadline() == null) {
            double value = IndicatorUtils.isMissingBidDeadlineRedFlag(tender.getCountry()) ? 0d : 100d;
            return calculated(value);
        } else if (tender.getBidDeadline() != null && lot.getAwardDecisionDate() != null) {
            long periodLength = ChronoUnit.DAYS.between(lot.getAwardDecisionDate(), tender.getBidDeadline());

            HashMap<String, Object> metaData = new HashMap<>();
            metaData.put("decisionPeriodLength", periodLength);

            if (periodLength < 0) {
                return insufficient();
            } else {
                return IndicatorUtils.checkDecisionPeriod(tender.getCountry(), periodLength)
                    ? calculated(0d, metaData) : calculated(100d, metaData);
            }
        } else {
            return IndicatorUtils.hasAward(tender) ? insufficient() : undefined();
        }
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.INTEGRITY_DECISION_PERIOD.name();
    }
}
