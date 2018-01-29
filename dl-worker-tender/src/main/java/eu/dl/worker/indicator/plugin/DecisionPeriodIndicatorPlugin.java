package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
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
public class DecisionPeriodIndicatorPlugin extends BaseIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null || tender.getCountry() == null) {
            return insufficient();
        }

        if (tender.getBidDeadline() == null) {
            return isMissingBidDeadlineRedFlag(tender.getCountry()) ? calculated(0d) : calculated(100d);
        } else {
            // iterate over lots and pick the oldest awardDecisionDate
            LocalDate awardDecisionDate = null;
            if (tender.getLots() != null) {
                for (MasterTenderLot lot : tender.getLots()) {
                    if (lot.getAwardDecisionDate() != null) {
                        if (awardDecisionDate == null || awardDecisionDate.isAfter(lot.getAwardDecisionDate())) {
                            awardDecisionDate = lot.getAwardDecisionDate();
                        }
                    }
                }
            }

            if (awardDecisionDate == null) {
                return tender.getPublications() == null || tender.getPublications()
                        .stream()
                        .filter(p -> p.getIsIncluded() != null && p.getIsIncluded())
                        .noneMatch(p -> p.getFormType() == PublicationFormType.CONTRACT_AWARD)
                        ? undefined()
                        : insufficient();
            }

            Long decisionPeriodLength = ChronoUnit.DAYS.between(
                    LocalDate.from(tender.getBidDeadline()), awardDecisionDate);
            HashMap<String, Object> metaData = new HashMap<String, Object>();
            metaData.put("decisionPeriodLength", decisionPeriodLength);
            metaData.put("awardDecisionDate", awardDecisionDate);
            metaData.put("bidDeadline", tender.getBidDeadline());
            if (decisionPeriodLength.compareTo(0L) < 0) {
                // the period is negative
                return insufficient(metaData);
            } else {
                return checkDecisionPeriod(tender.getCountry(), decisionPeriodLength)
                        ? calculated(0d, metaData)
                        : calculated(100d, metaData);
            }
        }
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.INTEGRITY_DECISION_PERIOD.name();
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
