package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
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
                .setTitle(JsoupUtils.selectText(String.format(SECTION_SELECTOR_PATTERN, "I.2"), form))
                .setBidDeadline(getSectionContent("I.8", form))
                .setCpvs(parseTenderCpvs("I.5", form))
                .setHasLots(parseIfTenderHasLots("I.6", form));

        return tender;
    }

}
