package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.apache.commons.lang3.StringUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static eu.datlab.worker.cz.parsed.VVZTenderParserUtils.getFieldValue;

import java.util.ArrayList;
import java.util.List;

/**
 * Handler for parsing form CZ01 - Předběžné oznámení podlimitního zadávacího řízení (Prior information notice for
 * under-the-threshold tenders).
 */
final class VVZFormCZ01Handler extends VVZCzPriorAndContractNoticeHandler {
    private static final Logger logger = LoggerFactory.getLogger(VVZFormCZ01Handler.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZFormCZ01Handler() {
        throw new AssertionError();
    }

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
    public static ParsedTender parseFormAttributes(final ParsedTender tender, final Document form) {
        ParsedTender parsedTender = parseCommonPriorAndContractNoticeAttributes(tender, form);

        // subsection II.5)
        parsedTender.setLots(parseLots(form));

        // SECTION III
        final Element sectionIII = VVZTenderParser.getSectionIII(form);

        // subsection III.3.3)
        parsedTender.addPublication(parseEstimatedContractNoticePublication(sectionIII));

        return parsedTender;
    }

    // ---------------------------------
    // CZ SUBSECTION II.5)
    // ---------------------------------

    /**
     * Parses lots.
     *
     * @param form
     *         form html
     *
     * @return parsed lots
     */
    private static List<ParsedTenderLot> parseLots(final Document form) {
        List<ParsedTenderLot> lots = new ArrayList<>();

        Elements lotsHtmls = VVZPriorAndContractNoticeHandler.getLotsHtmls(form);
        for (Element lotHtml : lotsHtmls) {
            ParsedTenderLot parsedLot = parseCommonLotAttributes(lotHtml, lotsHtmls.indexOf(lotHtml) + 1);
            lots.add(parsedLot);
        }
        return lots;
    }

    // ---------------------------------
    // CZ SUBSECTION II.5.6)
    // ---------------------------------

    // ---------------------------------
    // SUBSECTION III.3.3)
    // ---------------------------------

    /**
     * Parses information about estimated publication of future contract notice.
     *
     * @param sectionIII
     *         section III html
     *
     * @return info about estimated future contract notice
     */
    private static ParsedPublication parseEstimatedContractNoticePublication(final Element sectionIII) {
        final String estimatedContractNoticePublicationDate = getFieldValue(sectionIII, ".*\\.AwardScheduledDate$");
        if (StringUtils.isNotEmpty(estimatedContractNoticePublicationDate)) {
            return new ParsedPublication().setIsIncluded(false)
                    .setFormType(PublicationFormType.CONTRACT_NOTICE.toString())
                    .setPublicationDate(estimatedContractNoticePublicationDate);
        }
        return null;
    }
}
