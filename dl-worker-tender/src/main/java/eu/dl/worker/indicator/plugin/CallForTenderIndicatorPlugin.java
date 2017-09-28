package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.BasicEntityRelatedIndicator;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.util.HashSet;
import java.util.Set;

import static eu.dl.dataaccess.dto.indicator.TenderIndicatorType.CORRUPTION_PRIOR_INFORMATION_NOTICE;

/**
 * This plugin calculates prior information notice indicator.
 */
public class CallForTenderIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    /**
     * Application config instance.
     */
    private static final Config config = Config.getInstance();

    @Override
    public final Indicator evaulate(final MasterTender tender) {
        if (tender == null || tender.getCountry() == null) {
            return null;
        }

        Set<String> countries = config.getParamValueAsList("indicator.priorInformationNotice.redFlag",
                ",", HashSet.class);

        if (countries.contains(tender.getCountry())) {
            Boolean publicationFound = false;

            if (tender.getPublications() != null && !tender.getPublications().isEmpty()) {
                for (Publication publication: tender.getPublications()) {
                    if (publication.getFormType() == PublicationFormType.CONTRACT_NOTICE
                            || publication.getFormType() == PublicationFormType.PRIOR_INFORMATION_NOTICE) {
                        publicationFound = true;
                        break;
                    }
                }
            }

            if (!publicationFound) {
                Indicator indicator = new BasicEntityRelatedIndicator();
                indicator.setType(getType());
                return indicator;
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    @Override
    public final String getType() {
        return CORRUPTION_PRIOR_INFORMATION_NOTICE.name();
    }

}