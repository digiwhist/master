package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;

/**
 * This plugin calculates advertisement period length.
 */
public class AdvertisementPeriodIndicatorPlugin extends BaseIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null || tender.getCountry() == null) {
            return insufficient();
        }

        if (tender.getBidDeadline() == null) {
            return isMissingBidDeadlineRedFlag(tender.getCountry()) ? calculated(0d) : calculated(100d);
        } else {
            if (tender.getPublications() == null) {
                return insufficient();
            }

            // iterate over lots and pick the oldest callForTenderDate
            LocalDate oldestCallForTenderDate = null;
            for (Publication publication: tender.getPublications()) {
                if (publication.getFormType() != null
                        && publication.getFormType().equals(PublicationFormType.CONTRACT_NOTICE)
                        && publication.getPublicationDate() != null
                        && (oldestCallForTenderDate == null
                            || oldestCallForTenderDate.isAfter(publication.getPublicationDate()))) {
                        oldestCallForTenderDate = publication.getPublicationDate();
                }
            }

            if (oldestCallForTenderDate == null) {
                return insufficient();
            }

            Long advertisementPeriodLength = ChronoUnit.DAYS.between(
                    oldestCallForTenderDate, LocalDate.from(tender.getBidDeadline()));
            HashMap<String, Object> metaData = new HashMap<String, Object>();
            metaData.put("advertisementPeriodLength", advertisementPeriodLength);
            metaData.put("callForTenderDate", oldestCallForTenderDate);
            metaData.put("bidDeadline", tender.getBidDeadline());
            if (advertisementPeriodLength.compareTo(0L) < 0) {
                // the period is negative
                return insufficient(metaData);
            } else {
                    return checkAdvertisementPeriod(tender.getCountry(), advertisementPeriodLength)
                            ? calculated(0d, metaData)
                            : calculated(100d, metaData);
            }
        }
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.INTEGRITY_ADVERTISEMENT_PERIOD.name();
    }

    /**
     * Checks whether missing bidDeadline means red flag for a given country.
     * @param countryCode country code
     * @return true if missing bid deadline is considered red flag
     */
    private Boolean isMissingBidDeadlineRedFlag(final String countryCode) {
        // get configuration for given country
        String value = Config.getInstance().getParam("indicator." + countryCode + ".advertisementPeriod.missing");

        return value != null && value.equals("1");
    }

    /**
     * check whether the decision period is of problematic length.
     *
     * @param countryCode country code
     * @param advertisementPeriodLength decision period length
     *
     * @return true if the decision period length is considered problematic
     */
    private Boolean checkAdvertisementPeriod(final String countryCode, final Long advertisementPeriodLength) {
        String value = Config.getInstance().getParam(
                "indicator." + countryCode + ".advertisementPeriod.length");

        return IndicatorUtils.isValueInPeriod(value, advertisementPeriodLength);
    }
}
