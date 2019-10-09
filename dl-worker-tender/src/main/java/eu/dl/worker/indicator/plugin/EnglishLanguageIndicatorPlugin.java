package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import static eu.dl.dataaccess.dto.indicator.TenderIndicatorType.ADMINISTRATIVE_ENGLISH_AS_FOREIGN_LANGUAGE;

/**
 * This plugin calculates english as a foreign language indicator.
 */
public class EnglishLanguageIndicatorPlugin extends BaseIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null || tender.getCountry() == null || tender.getEligibleBidLanguages() == null
                || tender.getEligibleBidLanguages().isEmpty()) {
            return insufficient();
        }

        List<String> enLanguages = Arrays.asList("en", "english", "englisch", "ie", "uk");
        List<String> enCountries = Arrays.asList("ie", "uk", "en");

        if (enCountries.contains(tender.getCountry().toLowerCase())) {
            return undefined();
        }

        HashMap<String, Object> metaData = new HashMap<String, Object>();
        metaData.put("eligibleBidLanguages", tender.getEligibleBidLanguages());
        if (tender.getEligibleBidLanguages().stream().anyMatch(s -> enLanguages.contains(s.toLowerCase()))) {
            return calculated(100d, metaData);
        } else {
            return calculated(0d, metaData);
        }
    }

    @Override
    public final String getType() {
        return ADMINISTRATIVE_ENGLISH_AS_FOREIGN_LANGUAGE.name();
    }

}