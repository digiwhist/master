package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Element;

/**
 * Handler for parsing form "PZP".
 */
final class ENarocanjeContractNoticeHandler3 extends BaseENarocanjeFormInTableHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractNoticeHandler3() {}

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
        parseCommonFormInTableAttributes(tender, form);

        tender
                .setTitle(ENarocanjeTenderFormInTableUtils.getSectionContent("I.2", form))
                .setBidDeadline(ENarocanjeTenderFormInTableUtils.getSectionContent("I.8", form))
                .setCpvs(parseTenderCpvs("I.5", form))
                .setHasLots(parseIfTenderHasLots("I.6", form))
                .setSupplyType(parseSupplyType("I.3", form))
                .setDescription(ENarocanjeTenderFormInTableUtils.getSectionContent("I.4", form));

        return tender;
    }

}
