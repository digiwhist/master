package eu.datlab.worker.sk.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getFirstValueFromElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getValuesFromElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.parsePrice;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Parser for Uvo Tender old OZ form specific data.
 *
 * @author Michal Riha
 */
final class UvoTenderNewOzHandler {

    /**
     * Private constructor to make this class static.
     */
    private UvoTenderNewOzHandler() {
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
                    .setDescription(getFirstValueFromElement(lot, "div:has(span:containsOwn(Opis obstarávania)) + div"))
                    .setCpvs(parseLotNotMainCpvs(lot))
                    .setEstimatedPrice(parsePrice(lot,
                        "div:has(span:containsOwn(Odhadovaná hodnota)) + div > span + span + span",
                        "div:has(span:containsOwn(Odhadovaná hodnota)) + div > span",
                        "div:has(span:containsOwn(Odhadovaná hodnota)) + div > span + span", null))
                    .setEstimatedDurationInMonths(
                            getFirstValueFromElement(lot, "div:has(span:containsOwn(v mesiacoch)) + div > span"))
                    .setAddressOfImplementation(new ParsedAddress()
                        .addNuts(JsoupUtils.selectText("div:has(span:containsOwn(Miesto vykonania)) + div:has(span:containsOwn(Kód NUTS))" +
                            " + div", lot)))
                    .setAreVariantsAccepted(JsoupUtils.selectText("span:containsOwn(Budú sa akceptovať varianty) + span", lot))
                    .addFunding(new ParsedFunding()
                        .setIsEuFund(JsoupUtils.selectText("span:containsOwn(Obstarávanie sa týka projektu a/alebo programu financovaného" +
                            " z fondov Európskej únie) + span", lot))
                        .setProgramme(JsoupUtils.selectText("div:containsOwn(Číslo projektu alebo referenčné číslo) > span", lot)))
                    .setSelectionMethod(parseLotSelectionMethod(lot)));
        }

        return parsedLots.isEmpty() ? null : parsedLots;
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
     * Create element for each lot in OZ form.
     *
     * @param document
     *         element to be parsed from
     *
     * @return List<Element>
     */
    private static List<Element> getOzLotsSubsections(final Element document) {
        Element firstRootLine = document.select("span:matchesOwn(OPIS)").first();

        if (firstRootLine == null) {
            return null;
        }

        Element root = ParserUtils.getSubsectionOfElements(firstRootLine.parent(), null);
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

    /**
     * @param lot lot to be parsed
     *
     * @return List<ParsedCpv> or Null
     */
    private static List<ParsedCPV> parseLotNotMainCpvs(final Element lot) {
        final Element firstLineOfSubsection = lot.select("div:has(span.code:matchesOwn(^II\\.2\\.2[\\.\\)]))").first();
        final Element lastLineOfSubsection = lot.select("div:has(span.code:matchesOwn(^II\\.2\\.(3|4|5)[\\.\\)]))").first();

        if (firstLineOfSubsection == null || lastLineOfSubsection == null) {
            return null;
        }

        Element subsection = ParserUtils.getSubsectionOfElements(firstLineOfSubsection, lastLineOfSubsection);

        if (subsection == null) {
            return null;
        }

        List<String> cpvCodes = getValuesFromElement(subsection, "div.selectList > span:not(.title)");

        if (cpvCodes == null) {
            return null;
        }

        List<ParsedCPV> parsedCPVs = new ArrayList<>();

        for (String cpvCode : cpvCodes) {
            parsedCPVs.add(new ParsedCPV()
                .setCode(cpvCode.trim().replaceAll("\\.+$", ""))
                .setIsMain(String.valueOf(false)));
        }

        return parsedCPVs;
    }


    /**
     * Parse tender selection method from document.
     *
     * @param lot lot to be parsed
     *
     * @return String or Null
     */
    private static String parseLotSelectionMethod(final Element lot) {
        return getFirstValueFromElement(lot, new String[]{
            "span:matchesOwn((nomicky najvýhodnejšia ponuka z hľadiska|ajnižšia cena)):not(:matchesOwn(-|%|ritérium))" +
                ":not(:matchesOwn(^\\d))",
            "div:containsOwn(Kritéria kvality)",
            "span:matchesOwn(ižšie uvedené kritéri):not(:matchesOwn(-|%|ritérium)):not(:matchesOwn(^\\d))",
            "div:containsOwn(Kritériá kvality)",
            "div.subtitle:contains(Kritériá na vyhodnotenie ponúk) + div"
        });
    }
}
