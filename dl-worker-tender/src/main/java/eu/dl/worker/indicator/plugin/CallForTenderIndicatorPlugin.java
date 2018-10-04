package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.util.HashSet;
import java.util.List;

import static eu.dl.dataaccess.dto.indicator.TenderIndicatorType.INTEGRITY_CALL_FOR_TENDER_PUBLICATION;
import java.util.Set;

/**
 * This plugin calculates call for tenders publication indicator.
 */
public class CallForTenderIndicatorPlugin extends BaseIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    /**
     * Application config instance.
     */
    private static final Config config = Config.getInstance();

    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null || (tender.getCountry() == null && tender.getPublications() == null)) {
            return insufficient();
        }

        Set<String> countries = config.getParamValueAsList("indicator.priorInformationNotice.redFlag", ",", HashSet.class);

        if ((tender.getCountry() != null && tender.getPublications() == null)) {
            return countries.contains(tender.getCountry())
                    ? calculated(0d)
                    : calculated(100d);
        } else if (tender.getCountry() == null && tender.getPublications() != null) {
            return hasPriorInformationNoticeOrContractNotice(tender.getPublications())
                    ? calculated(100d)
                    : insufficient();
        } else {
            // country and publications are set

            if (countries.contains(tender.getCountry())) {
                return hasPriorInformationNoticeOrContractNotice(tender.getPublications())
                        ? calculated(100d)
                        : calculated(0d);
            } else {
                return calculated(100d);
            }
        }
    }

    @Override
    public final String getType() {
        return INTEGRITY_CALL_FOR_TENDER_PUBLICATION.name();
    }

    /**
     * Decides whether {@code publications} contains prior information notice or contract notice.
     *
     * @param publications
     *      list of publications
     * @return true if list of publications contains prior information notice or contract notice; otherwise false
     */
    private static boolean hasPriorInformationNoticeOrContractNotice(final List<Publication> publications) {
        return publications != null
                && publications
                .stream()
                .anyMatch(p -> p.getFormType() == PublicationFormType.CONTRACT_NOTICE
                        || p.getFormType() == PublicationFormType.PRIOR_INFORMATION_NOTICE);
    }
}