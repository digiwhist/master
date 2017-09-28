package eu.digiwhist.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Document;

/**
 * Handler for parsing form 11 - Contract notice - Contracts to be awarded by a concessionaire who is not a
 * contracting authority.
 */
public final class VestnikForm11Handler extends VestnikContractNoticeHandler {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VestnikForm11Handler() {
        throw new AssertionError();
    }

    /**
     * Parses Form specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from Form
     */
    public static ParsedTender parseFormAttributes(final ParsedTender tender, final Document form) {
        return parseCommonContractNoticeAttributes(tender, form);
    }
}
