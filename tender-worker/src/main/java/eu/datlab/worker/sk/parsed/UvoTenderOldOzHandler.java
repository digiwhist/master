package eu.datlab.worker.sk.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
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
 * Parser for Uvo Tender old OZ form specific data.
 *
 * @author Michal Riha
 */
final class UvoTenderOldOzHandler {

    /**
     * Private constructor to make this class static.
     */
    private UvoTenderOldOzHandler() {
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
        return parsedTender.setLots(parseTenderLots(document));
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
                    .setLotNumber(parseLotNumber(lot))
                    .setTitle(getFirstValueFromElement(lot, "div:has(span:containsOwn(NÁZOV)) + div"))
                    .setDescription(getFirstValueFromElement(lot, "div:has(span:containsOwn(STRUČNÝ OPIS)) + div"))
                    .addCpv(new ParsedCPV()
                            .setIsMain(String.valueOf(true))
                            .setCode(parseCpvCode(lot)))
                    .setEstimatedPrice(parseEstimatedLotPrice(lot))
                    .setEstimatedDurationInMonths(getFirstValueFromElement(lot, "div:has(span:containsOwn(v " +
                            "mesiacoch)) + div > span")));
        }

        return parsedLots.isEmpty() ? null : parsedLots;
    }

    /**
     * Parse estimated price for lot.
     *
     * @param lot lot to parse
     * @return ParsedPrice or null
     */
    private static ParsedPrice parseEstimatedLotPrice(final Element lot) {
        final String[] vatSelectors = new String[]{
                "span:containsOwn(Predpokladaná hodnota)"
        };

        final String[] priceSelectors = new String[]{
                "div:has(span:containsOwn(Predpokladaná hodnota)) + div > span",
                "div:has(span:containsOwn(MNOŽSTVO ALEBO ROZSAH)) + div + div:containsOwn(Hodnota) > span"
        };

        final String[] currencySelectors = new String[]{
                "div:has(span:containsOwn(Predpokladaná hodnota)) + div > span + span",
                "div:has(span:containsOwn(MNOŽSTVO ALEBO ROZSAH)) + div + div:containsOwn(Hodnota) > span + span"
        };



        return parsePrice(lot, vatSelectors, priceSelectors, currencySelectors, null);
    }

    /**
     * Parse lot number from lot.
     *
     * @param lot lot to parse from
     * @return String or null
     */
    private static String parseLotNumber(final Element lot) {
        final String lotNumber = getFirstValueFromElement(lot, "span:containsOwn(Časť:)");
        return lotNumber == null ? null : lotNumber.replace("Časť:", "");
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
        String cpvCode = getFirstValueFromElement(lot, "span:containsOwn(Hlavný slovník:)");

        if (cpvCode == null) {
            return null;
        } else {
            return cpvCode.replace("Hlavný slovník:", "").replaceAll("\\.+$", "");
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
        Element root = document.select("fieldset fieldset:has(legend:containsOwn(PRÍLOHA B))").first();

        if (root == null) {
            return null;
        }

        List<Element> lotFirstLines = root.select("span:containsOwn(Časť:)");

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
