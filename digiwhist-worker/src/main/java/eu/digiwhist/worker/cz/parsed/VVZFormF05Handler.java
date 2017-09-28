package eu.digiwhist.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Handler for parsing form F05 - Contract notice – utilities.
 */
final class VVZFormF05Handler extends VVZPriorAndContractNoticeHandler {
    private static final Logger logger = LoggerFactory.getLogger(VVZFormF05Handler.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZFormF05Handler() {
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

        // subsection I.6
        parsedTender.getBuyers().get(0).addMainActivity(parseBuyerMainActivityFromI6(sectionI));

        // subsection II.2
        parsedTender.setLots(parseContractNoticeLots(form));

        // subsection IV.1.1 (extended)
        parsedTender = parseAcceleratedProcedureTypeInfo(sectionIV, parsedTender);

        // subsection IV.2.1
        parsedTender.addPublication(parsePreviousTedPublication(sectionIV));

        // subsection IV.2.6
        parsedTender.setAwardDeadline(parseAwardDeadline(sectionIV))
            .setAwardDeadlineDuration(parseAwardDeadlineDuration(sectionIV));

        return parsedTender;
    }
}
