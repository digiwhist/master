package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.BasicEntityRelatedIndicator;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;

/**
 * This plugin calculates advertisement period length.
 */
public class AdvertisementPeriodIndicatorPlugin implements IndicatorPlugin<MasterTender> {

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
        } else if (tender.getPublications() != null) {
            // iterate over lots and pick the oldest callForTenderDate
            LocalDate callForTenderDate = null;
            for (Publication publication: tender.getPublications()) {
                if (publication.getFormType() != null
                        && publication.getFormType().equals(PublicationFormType.CONTRACT_NOTICE)) {
                    if (publication.getPublicationDate() != null && (
                            callForTenderDate == null
                            || callForTenderDate.isAfter(publication.getPublicationDate())
                        )
                    ) {
                        callForTenderDate = publication.getPublicationDate();
                    }
                }
            }

            if (callForTenderDate != null) {

                Long advertisementPeriodLength = ChronoUnit.DAYS.between(
                        callForTenderDate, LocalDate.from(tender.getBidDeadline()));
                if (checkAdvertisementPeriod(tender.getCountry(), advertisementPeriodLength)) {
                    // fill in metadata and return indicator
                    HashMap<String, Object> metaData = new HashMap<String, Object>();
                    metaData.put("advertisementPeriodLength", advertisementPeriodLength);
                    metaData.put("callForTenderDate", callForTenderDate);
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
        return TenderIndicatorType.CORRUPTION_ADVERTISEMENT_PERIOD.name();
    }

    /**
     * Checks whether missing bidDeadline means red flag for a given country.
     * @param countryCode country code
     * @return true if missing bid deadline is considered red flag
     */
    private Boolean isMissingBidDeadlineRedFlag(final String countryCode) {
        // get configuration for given country
        String value = Config.getInstance().getParam(
                "indicator." + countryCode + ".advertisementPeriod.missing");

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
