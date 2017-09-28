package eu.digiwhist.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Handler for parsing form F04 - Periodic indicative notice – utilities.
 */
final class VVZFormF04Handler extends VVZPriorAndContractNoticeHandler {
    private static final Logger logger = LoggerFactory.getLogger(VVZFormF04Handler.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZFormF04Handler() {
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

        final Element sectionI = VVZTenderParser.getSectionI(form);

        // subsection I.6
        parsedTender.getBuyers().get(0).addMainActivity(parseBuyerMainActivityFromI6(sectionI));

        // subsection II.2
        parsedTender.setLots(parseF04Lots(form));

        return parsedTender;
    }

    // =================================
    // SECTION II
    // =================================

    // ---------------------------------
    // SUBSECTION II.2)
    // ---------------------------------

    /**
     * Parses lots.
     *
     * @param form
     *         form html
     *
     * @return lots
     */
    private static List<ParsedTenderLot> parseF04Lots(final Document form) {
        List<ParsedTenderLot> lots = new ArrayList<>();

        Elements lotsHtml = getLotsHtmls(form);
        for (Element lotHtml : lotsHtml) {
            ParsedTenderLot parsedLot = parseCommonLotAttributes(lotHtml, lotsHtml.indexOf(lotHtml) + 1)
                    // subsection II.2.10)
                    .setAreVariantsAccepted(VVZTenderParser.parseAreVariantsAcceptedFromCheckbox(lotHtml))
                    // subsection II.2.11)
                    .setHasOptions(parseHasLotOptionsFromCheckbox(lotHtml));
            lots.add(parsedLot);
        }
        return lots;
    }
}
