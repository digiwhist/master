package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.apache.commons.lang.StringUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static eu.datlab.worker.cz.parsed.VVZTenderParserUtils.getFieldValue;

import java.util.ArrayList;
import java.util.List;

/**
 * Handler for parsing form F01 - Prior information notice.
 */
final class VVZFormF01Handler extends VVZPriorAndContractNoticeHandler {
    private static final Logger logger = LoggerFactory.getLogger(VVZFormF01Handler.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZFormF01Handler() {
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
        final Element sectionIV = VVZTenderParser.getSectionIV(form);

        // subsection I.4 and I.5
        parsedTender.getBuyers()
                .get(0)
                .setBuyerType(parseBuyerType(sectionI))
                .addMainActivity(parseBuyerMainActivity(sectionI));

        // subsection II.2
        parsedTender.setLots(parseLots(form));

        // subsection IV.2.5
        parsedTender.addPublication(parseEstimatedContractNoticePublication(sectionIV));

        return parsedTender;
    }

    /**
     * Parses lots.
     *
     * @param form
     *         form html
     *
     * @return list of lots
     */
    private static List<ParsedTenderLot> parseLots(final Document form) {
        List<ParsedTenderLot> lots = new ArrayList<>();

        Elements lotsHtml = getLotsHtmls(form);
        for (Element lotHtml : lotsHtml) {
            ParsedTenderLot parsedLot = parseCommonLotAttributes(lotHtml,
                    lotsHtml.indexOf(lotHtml) + 1).setAreVariantsAccepted(
                    VVZTenderParser.parseAreVariantsAcceptedFromCheckbox(lotHtml))
                    .setHasOptions(parseHasLotOptionsFromCheckbox(lotHtml));
            lots.add(parsedLot);
        }
        return lots;
    }

    // ---------------------------------
    // SUBSECTION IV.2.5)
    // ---------------------------------

    /**
     * Parses information about estimated publication of future contract notice.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return info about estimated future contract notice
     */
    private static ParsedPublication parseEstimatedContractNoticePublication(final Element sectionIV) {
        final String estimatedContractNoticePublicationDate = getFieldValue(sectionIV, ".*\\.AwardScheduled");
        if (StringUtils.isNotEmpty(estimatedContractNoticePublicationDate)) {
            return new ParsedPublication().setIsIncluded(false)
                    .setFormType(PublicationFormType.CONTRACT_NOTICE.toString())
                    .setPublicationDate(estimatedContractNoticePublicationDate);
        }
        return null;
    }
}
