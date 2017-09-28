package eu.digiwhist.worker.sk.parsed;

import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.getFirstValueFromElement;

import org.jsoup.nodes.Document;

import eu.dl.dataaccess.dto.parsed.ParsedTender;

/**
 * Parser for Uvo Tender ancient ZZZ form specific data.
 *
 * @author Michal Riha
 */
final class UvoTenderAncientZzzHandler {

    private static final String IN_PART_VI = "span.nadpis:matchesOwn(ODDIEL VI.*) + table ";

    /**
     * Private constructor to make this class static.
     */
    private UvoTenderAncientZzzHandler() {
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
                IN_PART_VI + "tr:has(span.nazov:containsOwn(DÁTUM ODOSLANIE TOHT)) + tr span.hodnota");
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
        return getFirstValueFromElement(document, new String[]{
                        IN_PART_VI + "tr:has(span.nazov:containsOwn(Informácie o zrušenom)) + tr span.hodnota",
                        IN_PART_VI + "tr:has(span:containsOwn(zrušenom postupe zadávania)) + tr span"
                });
    }
}
