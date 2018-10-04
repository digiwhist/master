package eu.datlab.worker.cz.master;

import eu.datlab.worker.master.BaseDatlabTenderMaster;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.master.plugin.generic.LogicalORPlugin;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Masters tender data for VVZ (new Vestnik).
 */
public class VVZTenderMaster extends BaseDatlabTenderMaster {

    private static final String VERSION = "1.0";

    @Override
    protected final void registerSpecificPlugins() {
        logger.debug("No specific plugins to be registered.");
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

    @Override
    protected final List<MatchedTender> sourceSpecificPreprocessData(final List<MatchedTender> items) {
        //exclude all except first published CONTRACT_AWARD from mastering if maste tender will be a FA
        // because the
        MasterTender tender = new MasterTender();
        List<MatchedTender> reducedList = new ArrayList<>();
        LogicalORPlugin plugin = new LogicalORPlugin(Arrays.asList("isFrameworkAgreement"));
        tender = (MasterTender) plugin.master(items, tender, items);
        if (tender.getIsFrameworkAgreement() != null && tender.getIsFrameworkAgreement()) {
            LocalDate minAwardPublicationDate = null;
            for (MatchedTender matchedTender : items) {
                if (isContractAward(matchedTender)) {
                    if (minAwardPublicationDate == null) {
                        minAwardPublicationDate = matchedTender.getPublicationDate();
                    } else if (minAwardPublicationDate.compareTo(matchedTender.getPublicationDate()) > 0) {
                        minAwardPublicationDate = matchedTender.getPublicationDate();
                    }
                }
            }
            
            for (MatchedTender matchedTender : items) {
                if (!isContractAward(matchedTender) || !isFromNewVestnik(matchedTender) 
                        || matchedTender.getPublicationDate().compareTo(minAwardPublicationDate) <= 0) {
                    reducedList.add(matchedTender);
                }
            }
        } else {
            reducedList = items;
        }

        List preprocessedData  = reducedList.stream()
                .filter(isNotContractAmendment())
                .collect(Collectors.toList());
        return preprocessedData;
    }

    /**
     * Tests whether matched tender comes from new www.vestnikverejnychzakazek.cz.
     * @param tender matched tender
     * @return TRUE if is created by eu.datlab.worker.cz.matched.VVZTenderMatcher
     */
    private boolean isFromNewVestnik(final MatchedTender tender) {
        if (tender.getCreatedBy().equals("eu.datlab.worker.cz.matched.VVZTenderMatcher")) {
            return true;
        } 
        
        return false;
    }
    
    /**
     * Tests whether matched tender is CONTRACT_AWARD or not.
     * @param tender matched tender
     * @return TRUE if included publication is of type CONTRACT_AWARD
     */
    private boolean isContractAward(final MatchedTender tender) {
        List<Publication> publications = tender.getPublications();
        if (publications != null) {
            for (Publication publication : publications) {
                if (publication.getIsIncluded() != null && publication.getIsIncluded().equals(Boolean.TRUE)
                        && publication.getFormType() != null && publication.getFormType().equals(PublicationFormType.CONTRACT_AWARD)) {
                    return true;
                }
            }
        }

        return false;
    }

    @Override
    protected final MasterTender sourceSpecificPostprocessData(final MasterTender item) {
        return item;
    }
}
