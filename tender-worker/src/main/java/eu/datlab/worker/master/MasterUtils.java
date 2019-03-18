package eu.datlab.worker.master;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * Class provides useful functions for mastering.
 * 
 * @author Tomas Mrazek
 */
public final class MasterUtils {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private MasterUtils() {
        throw new AssertionError();
    }

    /**
     * @param masterTender
     *      master tender
     * @param matchedTenders
     *      list of matched tenders belonging to master tender
     * @return reduced list of matched tenders
     */

    /**
     * Reduces input set of matched tenders so that only publications that are not considered CONTRACT IMPLEMENTATIONs remains.
     *
     * @param matchedTenders
     *      original set of all matched tenders for a given master tender
     * @param masterTender
     *      master tender which serves to get an information about isFrameworkAgreement
     * @return list of matched tenders that are not considered to be contract implementations
     */
    public static List<MatchedTender> reduceContractAwards(final List<MatchedTender> matchedTenders, final MasterTender masterTender) {
        List<MatchedTender> reducedList = new ArrayList<>();

        if (masterTender.getIsFrameworkAgreement() != null && masterTender.getIsFrameworkAgreement()) {
            LocalDate minAwardPublicationDate = null;
            for (MatchedTender matchedTender : matchedTenders) {
                if (isContractAward(matchedTender)) {
                    if (minAwardPublicationDate == null) {
                        minAwardPublicationDate = matchedTender.getPublicationDate();
                    } else if (minAwardPublicationDate.compareTo(matchedTender.getPublicationDate()) > 0) {
                        minAwardPublicationDate = matchedTender.getPublicationDate();
                    }
                }
            }

            for (MatchedTender matchedTender : matchedTenders) {
                if (!isContractAward(matchedTender) || !isFromNewVestnik(matchedTender)
                    || matchedTender.getPublicationDate().compareTo(minAwardPublicationDate) <= 0) {
                    reducedList.add(matchedTender);
                }
            }
        } else {
            reducedList = matchedTenders;
        }

        return reducedList;
    }

    /**
     * Tests whether matched tender comes from new www.vestnikverejnychzakazek.cz.
     * @param tender
     *      matched tender
     * @return TRUE if is created by eu.datlab.worker.cz.matched.VVZTenderMatcher
     */
    private static boolean isFromNewVestnik(final MatchedTender tender) {
        if (tender.getCreatedBy().equals("eu.datlab.worker.cz.matched.VVZTenderMatcher")) {
            return true;
        }

        return false;
    }

    /**
     * Tests whether matched tender is CONTRACT_AWARD or not.
     * @param tender
     *      matched tender
     * @return TRUE if included publication is of type CONTRACT_AWARD
     */
    private static boolean isContractAward(final MatchedTender tender) {
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
}
