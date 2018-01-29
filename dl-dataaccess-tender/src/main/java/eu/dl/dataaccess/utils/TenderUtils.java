package eu.dl.dataaccess.utils;

import java.io.UnsupportedEncodingException;
import java.util.List;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import java.time.LocalDate;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Class provides functions for tender manipulation.
 */
public final class TenderUtils {
    private static final Logger logger = LoggerFactory.getLogger(TenderUtils.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private TenderUtils() {
    }

    /**
     * Generates hash for the given matched tender.
     *
     * @param matchedTender
     *      matched tender
     * @return matched tender hash
     */
    public static String generateTenderHash(final MatchedTender matchedTender) {
        if (matchedTender == null) {
            return null;
        }

        String publication = "";

        try {
            List<Publication> cleanPublications = matchedTender.getPublications();
            for (Publication cleanPublication : cleanPublications) {
                if (cleanPublication.getIsIncluded()) {
                    publication = cleanPublication.getSourceTenderId();
                    break;
                }
            }

            if (publication == null || publication.isEmpty()) {
                // there is no source id assigned, the hash cannot be reasonably calculated
                publication = UUID.randomUUID().toString();
            }

            byte[] data = publication.getBytes("UTF-8");
            return org.apache.commons.codec.digest.DigestUtils.sha1Hex(data);
        } catch (UnsupportedEncodingException e) {
            logger.error("Unable to convert \"{}\" to UTF-8", publication);
            throw new UnrecoverableException("Unable to convert data to UTF-8", e);
        }
    }
    
    /**
     * Returns true is there is publication of type CONTRACT_AWARD.
     * 
     * @param tender tender to be checked
     * 
     * @return true if there is included publication of type contract award. 
     */
    public static Boolean hasContractAward(final MasterTender tender) {
    		if (tender != null && tender.getPublications() != null) {
    			for (Publication publication : tender.getPublications()) {
    				if (publication.getIsIncluded()
                            && publication.getFormType() == PublicationFormType.CONTRACT_AWARD) {
    					return true;
    				}
    			}
    		}
    		
    		return false;
    }

    /**
     * From given group selects latest published matched tenders of given form types.
     *
     * @param groupId
     *      tender group id
     * @param dao
     *      matched tender DAO
     * @param formTypes
     *      list of searched form types
     * @return map, where key includes the form type and value holds the found tender. If no tender is found returns
     *          an empty map
     */
    public static Map<PublicationFormType, MatchedTender> getLatestPublications(final String groupId,
        final MatchedTenderDAO dao, final List<PublicationFormType> formTypes) {
        if (groupId == null || dao == null || formTypes == null || formTypes.isEmpty()) {
            return Collections.emptyMap();
        }

        List<MatchedTender> group = dao.getByGroupId(groupId);
        Map<PublicationFormType, MatchedTender> latest = new HashMap<>();
        for (MatchedTender t : group) {
            if (t != null && t.getPublications() != null) {
                t.getPublications().stream()
                    .filter(p -> Objects.equals(p.getIsIncluded(), Boolean.TRUE) && p.getFormType() != null
                        && formTypes.contains(p.getFormType()))
                    .forEach(p -> {
                        MatchedTender current = latest.get(p.getFormType());

                        // publication date of included publication of the given type
                        LocalDate publicatonDate = null;
                        if (current != null && current.getPublications() != null) {
                            publicatonDate = current.getPublications().stream()
                                .filter(n -> Objects.equals(n.getIsIncluded(), Boolean.TRUE)
                                    && n.getFormType() == p.getFormType())
                                .findFirst().map(n -> n.getPublicationDate()).orElse(null);
                        }

                        // set latest
                        if (current == null
                            || (publicatonDate == null && p.getPublicationDate() != null)
                            || (publicatonDate != null && p.getPublicationDate() != null
                                && publicatonDate.isBefore(p.getPublicationDate()))) {
                            latest.put(p.getFormType(), t);
                        }
                    });
            }
        }

        return latest;
    }
}
