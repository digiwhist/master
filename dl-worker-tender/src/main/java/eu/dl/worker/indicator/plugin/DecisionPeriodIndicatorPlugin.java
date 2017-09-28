package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.indicator.BasicEntityRelatedIndicator;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;

/**
 * This plugin calculates decision period length.
 */
public class DecisionPeriodIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaulate(final MasterTender tender) {
        if (tender == null || tender.getCountry() == null) {
            return null;
        }

        Indicator indicator = new BasicEntityRelatedIndicator();
        indicator.setType(getType());

        if (tender.getBidDeadline() == null) {
            // no deadline specified
            if (isMissingBidDeadlineRedFlag(tender.getCountry())) {
                return indicator;
            }
        } else if (tender.getLots() != null) {
            // iterate over lots and pick the oldest awardDecisionDate
            LocalDate awardDecisionDate = null;
            for (MasterTenderLot lot : tender.getLots()) {
                if (lot.getAwardDecisionDate() != null) {
                    if (awardDecisionDate == null || awardDecisionDate.isBefore(lot.getAwardDecisionDate())) {
                        awardDecisionDate = lot.getAwardDecisionDate();
                    }
                }
            }

            if (awardDecisionDate != null) {

                Long decisionPeriodLength = ChronoUnit.DAYS.between(
                        LocalDate.from(tender.getBidDeadline()), awardDecisionDate);
                if (checkDecisionPeriod(tender.getCountry(), decisionPeriodLength)) {
                    // fill in metadata and return indicator
                    HashMap<String, Object> metaData = new HashMap<String, Object>();
                    metaData.put("decisionPeriodLEngth", decisionPeriodLength);
                    metaData.put("awardDecisionDate", awardDecisionDate);
                    metaData.put("bidDeadline", tender.getBidDeadline());
                    indicator.setMetaData(metaData);

                    return indicator;
                }
            }
        }

        return null;
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.CORRUPTION_DECISION_PERIOD.name();
    }

    /**
     * Checks whether missing bidDeadline means red flag for a given country.
     * @param countryCode country code
     * @return true if missing bid deadline is considered red flag
     */
    private Boolean isMissingBidDeadlineRedFlag(final String countryCode) {
        // get configuration for given country
        String value = Config.getInstance().getParam(
                "indicator." + countryCode + ".decisionPeriod.missing");

        return value != null && value.equals("1");
    }

    /**
     * check whether the decision period is of problematic length.
     *
     * @param countryCode country code
     * @param decisionPeriodLength decision period length
     *
     * @return true if the decision period length is considered problematic
     */
    private Boolean checkDecisionPeriod(final String countryCode, final Long decisionPeriodLength) {
        String value = Config.getInstance().getParam(
                "indicator." + countryCode + ".decisionPeriod.length");

        return IndicatorUtils.isValueInPeriod(value, decisionPeriodLength);
    }
}
