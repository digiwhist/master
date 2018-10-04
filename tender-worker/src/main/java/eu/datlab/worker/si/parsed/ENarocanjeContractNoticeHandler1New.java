package eu.datlab.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Element;

/**
 * Handler for parsing new forms "EU 2 - SL" and "EU 5 - SL".
 */
final class ENarocanjeContractNoticeHandler1New extends BaseENarocanjeContractNoticeInDivsHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractNoticeHandler1New() {}

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
