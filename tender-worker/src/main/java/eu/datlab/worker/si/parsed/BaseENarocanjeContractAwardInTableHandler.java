package eu.datlab.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Element;

/**
 * Base handler for parsing old forms "EU 3 - SL", "EU 6 - SL" and "NMV2".
 */
abstract class BaseENarocanjeContractAwardInTableHandler extends BaseENarocanjeFormInTableHandler {
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
    public static ParsedTender parseCommonContractAwardAttributes(final ParsedTender tender, final Element form) {
        parseCommonFormInTableAttributes(tender, form);

        tender
                // "NMV2" has title in I.2). Fix it when we have annotation
                .setTitle(ENarocanjeTenderFormInTableUtils.getSectionContent("II.1.1", form));

        return tender;
    }

}
