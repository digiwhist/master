package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;

/**
 * Handler for parsing old forms "EU 2 - SL", "EU 5 - SL" and "NMV1".
 */
final class ENarocanjeContractNoticeHandler1Old extends BaseENarocanjeFormInTableHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractNoticeHandler1Old() {}

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
    public static ParsedTender parse(final ParsedTender tender, final Element form) {
        parseCommonAttributes(tender, form);

        tender
                .setTitle(JsoupUtils.selectText(String.format(SECTION_SELECTOR_PATTERN, "II.1.1"), form));

        return tender;
    }

}
