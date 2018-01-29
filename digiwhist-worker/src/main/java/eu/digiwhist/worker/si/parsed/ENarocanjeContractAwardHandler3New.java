package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;

/**
 * Handler for parsing new forms "EU 6 - SL" and "NMV2".
 */
final class ENarocanjeContractAwardHandler3New extends BaseENarocanjeContractAwardInDivsHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractAwardHandler3New() {}

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

        final Element sectionI6 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_I_6_TITLE_SELECTOR, form),
                null);

        tender.getBuyers().get(0)
                // section I.6 is not always presented. See
                // https://www.enarocanje.si/Obrazci/?id_obrazec=211849
                .addMainActivity(sectionI6 == null ? null : sectionI6.ownText());

        return tender;
    }

}
