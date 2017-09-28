package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.BasicEntityRelatedIndicator;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.util.Arrays;
import java.util.List;

import static eu.dl.dataaccess.dto.indicator.TenderIndicatorType.ADMINISTRATIVE_ENGLISH_AS_FOREIGN_LANGUAGE;

/**
 * This plugin calculates english as a foreign language indicator.
 */
public class EnglishLanguageIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaulate(final MasterTender tender) {
        if (tender == null || tender.getEligibleBidLanguages() == null || tender.getEligibleBidLanguages().isEmpty()) {
            return null;
        }

        List<String> enVersions = Arrays.asList("en", "english", "englisch");
        if (tender.getEligibleBidLanguages().stream().anyMatch(s -> enVersions.contains(s.trim().toLowerCase()))) {

            Indicator indicator = new BasicEntityRelatedIndicator();
            indicator.setType(getType());

            return indicator;
        } else {
            return null;
        }
    }

    @Override
    public final String getType() {
        return ADMINISTRATIVE_ENGLISH_AS_FOREIGN_LANGUAGE.name();
    }

}