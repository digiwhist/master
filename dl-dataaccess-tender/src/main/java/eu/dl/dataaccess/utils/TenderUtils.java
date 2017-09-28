package eu.dl.dataaccess.utils;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.util.List;
import java.util.UUID;

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

}
