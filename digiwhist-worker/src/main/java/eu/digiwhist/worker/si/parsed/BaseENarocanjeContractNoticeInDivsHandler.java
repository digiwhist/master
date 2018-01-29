package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;

/**
 * Base handler for parsing new contract notices.
 */
abstract class BaseENarocanjeContractNoticeInDivsHandler extends BaseENarocanjeFormInDivsHandler {
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
    public static ParsedTender parseCommonContractNoticeAttributes(final ParsedTender tender, final Element form) {
        parseCommonFormInDivsAttributes(tender, form);

        final Element sectionIV23TitleElement = JsoupUtils.selectFirst(SUBSECTION_IV_2_3_TITLE_SELECTOR, form);
        final Element sectionIV26TitleElement = JsoupUtils.selectFirst(SUBSECTION_IV_2_6_TITLE_SELECTOR, form);
        final Element sectionII15 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_II_1_5_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_II_1_6_TITLE_SELECTOR, form));
        final Element sectionII13 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_II_1_3_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_II_1_4_TITLE_SELECTOR, form));
        final Element sectionIV22 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_IV_2_2_TITLE_SELECTOR, form),
                sectionIV23TitleElement == null ? sectionIV26TitleElement : sectionIV23TitleElement);
        final Element sectionIV26 = ParserUtils.getSubsectionOfNodes(
                sectionIV26TitleElement,
                JsoupUtils.selectFirst(SUBSECTION_IV_2_7_TITLE_SELECTOR, form));

        tender
            .setEstimatedPrice(parseEstimatedPrice(sectionII15))
            .setBidDeadline(sectionIV22.ownText())
            .setAwardDeadline(ParserUtils.getFromContent(sectionIV26, null, "Ponudba mora biti veljavna do"))
            .setSupplyType(sectionII13.ownText());

        return tender;
    }

}
