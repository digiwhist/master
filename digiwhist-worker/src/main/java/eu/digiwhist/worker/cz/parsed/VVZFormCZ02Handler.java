package eu.digiwhist.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.List;

/**
 * Handler for parsing form CZ02 - Oznámení o zahájení podlimitního zadávacího řízení (Contract notice for
 * under-the-threshold tenders).
 */
final class VVZFormCZ02Handler extends VVZCzPriorAndContractNoticeHandler {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZFormCZ02Handler() {
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

        // subsection III.1.2)
        parsedTender = parseEnvisagedCandidatesCountInfo(sectionIII, parsedTender);

        return parsedTender;
    }

    // ---------------------------------
    // CZ SUBSECTION II.5)
    // ---------------------------------

    /**
     * Parses info about lots.
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

            // subsection II.5.6)
            parsedLot.setAwardCriteria(parseAwardCriteria(lotHtml));

            // subsection II.5.7)
            // parsedLot.setBidDeadline(parseBidDeadline(lotHtml));

            lots.add(parsedLot);
        }
        return lots;
    }

    // ---------------------------------
    // CZ SUBSECTION III.1.2)
    // ---------------------------------

    /**
     * Parses envisaged candidates count info.
     *
     * @param lotHtml
     *         lot html
     * @param tender
     *         tender to be updated
     *
     * @return updated tender with envisaged candidates count info
     */
    private static ParsedTender parseEnvisagedCandidatesCountInfo(final Element lotHtml, final ParsedTender tender) {
        return tender.setEnvisagedCandidatesCount(parseEnvisagedCandidatesCount(lotHtml))
                .setEnvisagedMinCandidatesCount(parseEnvisagedMinCandidatesCount(lotHtml))
                .setEnvisagedMaxCandidatesCount(parseEnvisagedMaxCandidatesCount(lotHtml));
    }

    /**
     * Parses envisaged candidates count.
     *
     * @param lotHtml
     *         lot html
     *
     * @return envisaged candidates count
     */
    private static String parseEnvisagedCandidatesCount(final Element lotHtml) {
        return VVZTenderParserUtils.getFieldValue(lotHtml, ".*EnvisagedCandidate$");
    }

    /**
     * Parses envisaged minimum candidates count.
     *
     * @param lotHtml
     *         lot html
     *
     * @return envisaged minimum candidates count
     */
    private static String parseEnvisagedMinCandidatesCount(final Element lotHtml) {
        return VVZTenderParserUtils.getFieldValue(lotHtml, ".*LimitCandidateMin$");
    }

    /**
     * Parses envisaged maximum candidates count.
     *
     * @param lotHtml
     *         lot html
     *
     * @return envisaged maximum candidates count
     */
    private static String parseEnvisagedMaxCandidatesCount(final Element lotHtml) {
        return VVZTenderParserUtils.getFieldValue(lotHtml, ".*LimitCandidateMax$");
    }
}
