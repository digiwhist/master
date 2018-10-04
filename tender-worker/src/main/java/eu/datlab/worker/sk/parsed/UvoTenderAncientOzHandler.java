package eu.datlab.worker.sk.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getFirstValueFromElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.parsePrice;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Parser for Uvo Tender ancient OZ form specific data.
 *
 * @author Michal Riha
 */
final class UvoTenderAncientOzHandler {
    /**
     * Private constructor to make this class static.
     */
    private UvoTenderAncientOzHandler() {
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
                .setLots(parseTenderLots(document));
    }

    /**
     * Parse tender lots from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return List<ParsedBasicTenderLot> or Null
     */
    private static List<ParsedTenderLot> parseTenderLots(final Document document) {
        List<Element> lots = getOzLotsSubsections(document);

        if (lots == null) {
            return null;
        }

        final List<ParsedTenderLot> parsedLots = new ArrayList<>();

        int positionOnPage = 1;
        for (Element lot : lots) {
            parsedLots.add(new ParsedTenderLot()
                    .setPositionOnPage(String.valueOf(positionOnPage++))
                    .setLotNumber(
                            getFirstValueFromElement(lot, "span:containsOwn(čast) ~ span.hodnota"))
                    .setTitle(
                            getFirstValueFromElement(lot,   "span.nazov:containsOwn(NÁZOV) ~ span.hodnota"))
                    .setDescription(
                            getFirstValueFromElement(lot, "span.nazov:containsOwn(STRUČNÝ OPIS) ~ span.hodnota"))
                    .addCpv(new ParsedCPV()
                            .setIsMain(String.valueOf(true))
                            .setCode(parseCpvCode(lot)))
                    .setEstimatedPrice(parsePrice(lot, false,
                            "span:containsOwn(Hodnota) ~ span.hodnota",
                            "span:containsOwn(Hodnota) ~ span.hodnota ~ span.hodnota", null))
                    .setEstimatedDurationInMonths(
                            getFirstValueFromElement(lot, "tr:contains(Obdobie: v mesiacoch) + tr")));
        }

        return parsedLots.isEmpty() ? null : parsedLots;
    }

    /**
     * Parse cpv code.
     *
     * @param lot
     *         lot to parse from
     *
     * @return parsed cpv code
     */
    private static String parseCpvCode(final Element lot) {
        String cpvCode = getFirstValueFromElement(lot, "span.hodnota:containsOwn(Hlavný slovník:) ~ span.hodnota");

        if (cpvCode == null) {
            return null;
        } else {
            return cpvCode.replaceAll("\\.+$", "");
        }
    }

    /**
     * Create element for each lot in OZ form.
     *
     * @param document
     *         element to be parsed from
     *
     * @return List<Element>
     */
    private static List<Element> getOzLotsSubsections(final Element document) {
        Element root = document.select("tr > td:has(span.nadpis:containsOwn(PRÍLOHA))").first();

        if (root == null) {
            return null;
        }

        List<Element> lotFirstLines = root.select(
                "tr:not(:has(table)):has(td:has" + "(span:containsOwn(Poradové číslo časti)))");

        if (lotFirstLines == null || lotFirstLines.isEmpty()) {
            return Arrays.asList(root);
        }

        // Few publications have multiple designations of one lot, remove those
        String previousLine = "";
        for (int iterator = 0; iterator < lotFirstLines.size();) {
            if (lotFirstLines.get(iterator).toString().equals(previousLine)) {
                lotFirstLines.remove(iterator);
            } else {
                previousLine = lotFirstLines.get(iterator).toString();
                iterator++;
            }
        }

        List<Element> subsections = new ArrayList<>();

        for (int iterator = 0; iterator < lotFirstLines.size(); iterator++) {
            if ((iterator + 1) != lotFirstLines.size()) {
                subsections.add(ParserUtils.getSubsectionOfElements(lotFirstLines.get(iterator),
                        lotFirstLines.get(iterator + 1)));
            } else {
                subsections.add(ParserUtils.getSubsectionOfElements(lotFirstLines.get(iterator), null));
            }
        }

        return subsections;
    }
}
