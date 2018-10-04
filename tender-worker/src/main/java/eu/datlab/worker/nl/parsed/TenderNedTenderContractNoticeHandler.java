package eu.datlab.worker.nl.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Element;

import java.util.Arrays;
import java.util.List;

/**
 * Contract notice form parser for TenderNed in Netherlands.
 *
 * @author Marek Mikes
 */
final class TenderNedTenderContractNoticeHandler {
    /**
     * Private constructor to make this class static.
     */
    private TenderNedTenderContractNoticeHandler() {}

    /**
     * Parses data for all contract notice forms.
     *
     * @param parsedTender
     *         tender to add data to
     * @param form
     *         document to parse data from
     *
     * @return List<ParsedTender> with parsed data
     */
    static List<ParsedTender> parse(final ParsedTender parsedTender, final Element form) {
        switch (TenderNedTenderFormUtils.getFormAge(form)) {
            case ANCIENT:
                return Arrays.asList(TenderNedTenderAncientContractNoticeHandler.parse(parsedTender, form));
            case OLD:
                return Arrays.asList(TenderNedTenderOldContractNoticeHandler.parse(parsedTender, form));
            case NEW:
                return Arrays.asList(TenderNedTenderNewContractNoticeHandler.parse(parsedTender, form));
            default:
                return Arrays.asList(parsedTender);
        }
    }
}
