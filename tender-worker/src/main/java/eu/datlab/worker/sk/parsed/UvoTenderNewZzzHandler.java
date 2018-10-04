package eu.datlab.worker.sk.parsed;

import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getFirstValueFromElement;

import org.jsoup.nodes.Document;

import eu.dl.dataaccess.dto.parsed.ParsedTender;

/**
 * Parser for Uvo Tender old ZZZ form specific data.
 *
 * @author Michal Riha
 */
final class UvoTenderNewZzzHandler {
    private static final String IN_PART_VI = "span.nadpis:matchesOwn(ODDIEL VI.*) + table ";

    /**
     * Private constructor to make this class static.
     */
    private UvoTenderNewZzzHandler() {
    }

    /**
     * Parse method for Uvo Tender OZ form specific data.
     *
     * @param parsedTender
     *         tender to add data to
     * @param document
     *         document to parse data from
     *
     * @return ParsedBasicTender with data added
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        return parsedTender
                .setCancellationDate(parseCancelationDate(document))
                .setCancellationReason(parseCancelationReason(document))
                .setIsWholeTenderCancelled(String.valueOf(true));
    }

    /**
     * Parse tender cancelation date value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseCancelationDate(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_VI + "div:has(span:containsOwn(Dátum odoslania tohto)) + div");
    }

    /**
     * Parse tender cancelation reason value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseCancelationReason(final Document document) {
        return getFirstValueFromElement(document,
                IN_PART_VI + "div:has(span:containsOwn(Postup bol zrušený podľa zákon))" + " + div:not(:contains"
                        + "(DÁTUM ODOSLANIA TOHTO OZNÁMENIA))");
    }
}
