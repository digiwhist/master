package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Element;

/**
 * Handler for parsing new forms "NMV1".
 */
final class ENarocanjeContractNoticeHandler4New extends BaseENarocanjeContractNoticeInDivsHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractNoticeHandler4New() {}

    /**
     * Parses form specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from form
     */
    public static ParsedTender parse(final ParsedTender tender, final Element form) {
        parseCommonContractNoticeAttributes(tender, form);

        return tender;
    }

}
