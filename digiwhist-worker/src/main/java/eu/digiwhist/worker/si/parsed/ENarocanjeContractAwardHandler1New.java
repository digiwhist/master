package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;

/**
 * Handler for parsing new forms "EU 3 - SL".
 */
final class ENarocanjeContractAwardHandler1New extends BaseENarocanjeContractAwardInDivsHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractAwardHandler1New() {}

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
        parseCommonContractAwardAttributes(tender, form);

        final Element sectionI5 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_I_5_TITLE_SELECTOR, form),
                null);

        tender.getBuyers().get(0)
                .addMainActivity(sectionI5 == null ? null : sectionI5.ownText());

        return tender;
    }

}
