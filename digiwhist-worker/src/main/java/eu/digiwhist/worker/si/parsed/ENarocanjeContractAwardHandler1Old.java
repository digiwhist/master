package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;

/**
 * Handler for parsing old forms "EU 3 - SL", "EU 6 - SL" and "NMV2".
 */
final class ENarocanjeContractAwardHandler1Old extends BaseENarocanjeFormInTableHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractAwardHandler1Old() {}

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
                // "NMV2" has title in I.2). Fix it when we have annotation
                .setTitle(JsoupUtils.selectText(String.format(SECTION_SELECTOR_PATTERN, "II.1.1"), form));

        return tender;
    }

}
