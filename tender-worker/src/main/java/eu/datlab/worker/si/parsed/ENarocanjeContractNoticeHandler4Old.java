package eu.datlab.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.StringUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;

/**
 * Handler for parsing old forms "NMV1".
 */
final class ENarocanjeContractNoticeHandler4Old extends BaseENarocanjeFormInTableHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractNoticeHandler4Old() {}

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
        // do not parse the form when it is a low value CFT. It has different structure from other NMV1 forms. See
        // https://www.enarocanje.si/Obrazci/?id_obrazec=29402
        if (JsoupUtils.exists(
                "div.panel-heading.panel-title > span > small:containsOwn(Obvestilo o naročilu male vrednosti)",
                form)) {
            return tender;
        }

        parseCommonFormInTableAttributes(tender, form);

        tender
                .setTitle(ENarocanjeTenderFormInTableUtils.getSectionContent("II.1.1", form))
                .setSupplyType(parseSupplyType("I.3", form))
                .setDescription(ENarocanjeTenderFormInTableUtils.getSectionContent("I.5", form))
                .setCpvs(parseTenderCpvs("I.6", form))
                .setHasLots(parseIfTenderHasLots("I.7", form))
                .setSelectionMethod(StringUtils.removeDotsAtTheEnd(ENarocanjeTenderFormInTableUtils.getSectionContent(
                        "I.8", form)))
                .setBidDeadline(ENarocanjeTenderFormInTableUtils.getSectionContent("I.10", form));

        return tender;
    }

}
