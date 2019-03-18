package eu.datlab.worker.sk.parsed;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getFirstElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getFirstValueFromElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.parsePrice;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Parser for Uvo Tender ancient OZZ form specific data.
 *
 * @author Michal Riha
 */
final class UvoTenderAncientOzzHandler {

    private static final String IN_PART_II = "span.nadpis:matchesOwn(ODDIEL II.*) + table";

    /**
     * Private constructor to make this class static.
     */
    private UvoTenderAncientOzzHandler() {
    }

    /**
     * Parse method for Uvo Tender OZZ form specific data.
     *
     * @param parsedTender tender to add data to
     * @param document     document to parse data from
     * @return ParsedBasicTender with data added
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        Element partII = getFirstElement(document, IN_PART_II);
        return parsedTender
                .setFinalPrice(parseFinalPrice(partII))
                .setLots(parseLots(document));
    }

    /**
     * Parse final price.
     *
     * @param document document to prase from
     * @return ParsedPrice or null
     */
    private static ParsedPrice parseFinalPrice(final Element document) {
        final String[] vatSelectors = new String[]{
                "tr:has(td.kod:matchesOwn(^II\\.2\\.1[\\.\\)])) ~ tr:matches(Hodnota) + tr"
        };

        final String[] priceSelectors = new String[]{
                "tr:has(td.kod:matchesOwn(^II\\.2\\.1[\\.\\)])) ~ tr:matches(Hodnota) span.hodnota"
        };

        final String[] currencySelectors = new String[]{
                "tr:has(td.kod:matchesOwn(^II\\.2\\.1[\\.\\)])) ~ tr:matches(Hodnota) span.hodnota + span.hodnota"
        };

        final String[] vatAmountSelectors = new String[]{
                "tr:has(td.kod:matchesOwn(^II\\.2\\.1[\\.\\)])) ~ tr:matches(Hodnota) + tr + tr span.hodnota",
                "tr:has(td.kod:matchesOwn(^II\\.2\\.1[\\.\\)])) ~ tr:matches(Hodnota) + tr + tr"
        };

        ParsedPrice result = parsePrice(document, vatSelectors, priceSelectors, currencySelectors, vatAmountSelectors);

        if (result == null) {
            Element minAmount = document.selectFirst("span:containsOwn(Najnižšia ponuka) ~ span.hodnota");
            Element maxAmount = document.selectFirst("span:containsOwn(najvyššia ponuka) ~ span.hodnota");
            Element currency = document.selectFirst("span:containsOwn(najvyššia ponuka) ~ span.hodnota + span.hodnota");

            if (minAmount != null || maxAmount != null) {
                result = new ParsedPrice()
                        .setMinNetAmount(minAmount == null ? null : minAmount.text())
                        .setMaxNetAmount(maxAmount == null ? null : maxAmount.text())
                        .setCurrency(currency == null ? null : currency.text());
            }
        }

        return result;
    }

    /**
     * Parse tender lots from element.
     *
     * @param document element to parse from
     * @return List<ParsedBasicTenderLot> or null
     */
    private static List<ParsedTenderLot> parseLots(final Document document) {
        List<Element> lots = getOzzLotsSubsections(document);

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
                    .setTitle(getFirstValueFromElement(lot, "span:containsOwn(Názov) ~ span.hodnota"))
                    .setAwardDecisionDate(getFirstValueFromElement(lot, new String[]{
                            "span.nazov:containsOwn(DÁTUM UZATVORENIA ZMLUVY) ~ span.hodnota",
                            "span.nazov:containsOwn(DÁTUM ROZHODNUTIA) ~ span.hodnota"}))
                    .setBidsCount(getFirstValueFromElement(lot, new String[]{
                            "span:containsOwn(POČET PRIJATÝCH PONÚK) ~ span.hodnota",
                            "span:containsOwn(POČET DORUČENÝCH PONÚK) ~ span.hodnota"}))
                    .setElectronicBidsCount(getFirstValueFromElement(lot,
                            "span:containsOwn(ponúk prijatých elektronickou cestou) ~ span.hodnota"))
                    .addBid(parsedBid(lot))
                    .setEstimatedPrice(parseEstimatedPrice(lot))
                    .setEstimatedDurationInMonths(getFirstValueFromElement(lot, "tr:has(span:containsOwn(hodnote " +
                            "za rok alebo za mesiac) ~ span.hodnota:containsOwn(mesiacov)) + tr span.hodnota"))
                    .setEstimatedDurationInYears(getFirstValueFromElement(lot, "tr:has(span:containsOwn(hodnote " +
                            "za rok alebo za mesiac) ~ span.hodnota:containsOwn(rokov)) + tr span.hodnota"))
            );
        }

        return parsedLots.isEmpty() ? null : parsedLots;
    }

    /**
     * Parse lot bid.
     *
     * @param lot lot to parse from
     * @return ParsedBid or null
     */
    private static ParsedBid parsedBid(final Element lot) {
        ParsedBid parsedBid = new ParsedBid()
                .addBidder(parseBidder(lot))
                .setIsSubcontracted(getFirstValueFromElement(lot, "span:containsOwn(JESTVUJE PREDPOKLAD " +
                        "SUBDODÁVOK) ~ span.hodnota"))
                .setIsWinning(String.valueOf(true))
                .setPrice(parsePrice(lot,
                        "tr:has(td > span:containsOwn(Celková konečná" + " hodnota zákazky)) + " +
                                "tr + tr",
                        "tr:has(td > span:containsOwn(Celková konečná" + " hodnota zákazky)) + " +
                                "tr span.hodnota",
                        "tr:has(td > span:containsOwn(Celková konečná" + " hodnota zákazky)) + " +
                                "tr span.hodnota + span.hodnota",
                        "span:containsOwn(Sadzba DPH) ~ span.hodnota"));

        if (parsedBid.getBidders() == null && parsedBid.getPrice() == null) {
            return null;
        } else {
            return parsedBid;
        }
    }

    /**
     * Parse lot bidder.
     *
     * @param lot to be parsed
     * @return ParsedBody or null
     */
    private static ParsedBody parseBidder(final Element lot) {
        ParsedBody parsedBody = new ParsedBody()
                .setName(getFirstValueFromElement(lot, new String[]{
                        "td.hodnota:containsOwn(Úradný názov:) span.hodnota",
                        "table.table span.hodnota"}))
                .setAddress(new ParsedAddress()
                        .setStreet(getFirstValueFromElement(lot, "td.hodnota:containsOwn(Poštová " +
                                "adresa:) span.hodnota"))
                        .setPostcode(getFirstValueFromElement(lot, "td.hodnota:containsOwn(PSČ:) " +
                                "span.hodnota"))
                        .setCity(getFirstValueFromElement(lot, "td.hodnota:containsOwn" +
                                "(Mesto/obec:) span.hodnota"))
                        .setCountry(getFirstValueFromElement(lot, new String[]{
                                "td.hodnota:containsOwn(Štát:) span.hodnota",
                                "span:containsOwn(Slovensko)"
                        }))
                        .setRawAddress(getFirstValueFromElement(lot,
                                "table.table tr:has(td:containsOwn(IČO)) + tr"))
                        .setUrl(getFirstValueFromElement(lot, "span:containsOwn(\\(URL\\)) ~ span" +
                                ".hodnota")))
                .setPhone(getFirstValueFromElement(lot, "td.hodnota:containsOwn(Telefón:) span" +
                        ".hodnota"))
                .setEmail(getFirstValueFromElement(lot, "td.hodnota:containsOwn(E-mail:) span" +
                        ".hodnota"));

        final String bodyId = getFirstValueFromElement(lot, new String[]{
                "td.hodnota:containsOwn(IČO:) span.hodnota",
                "td.hodnota:containsOwn(Vnútroštátne identifikačné číslo:) span.hodnota"});

        if (bodyId != null) {
            parsedBody.setBodyIds(Arrays.asList(new BodyIdentifier()
                    .setId(bodyId)
                    .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                    .setScope(BodyIdentifier.Scope.SK)));
        }

        if (parsedBody.getName() == null && parsedBody.getPhone() == null
                && parsedBody.getEmail() == null && parsedBody.getBodyIds() == null) {
            return null;
        } else {
            return parsedBody;
        }
    }

    /**
     * Parse estimated price.
     *
     * @param lot lot to be parsed from
     * @return ParsedPrice or null
     */
    private static ParsedPrice parseEstimatedPrice(final Element lot) {
        final String[] priceSelectors = new String[]{
                "tr:has(span:containsOwn(Začiatočná predpokladaná celková hodnota zákazky)) + tr span.hodnota",
                "tr:has(span:containsOwn(INFORMÁCIE O HODNOTE ZÁKAZKY)) + tr:not(:has(span)) + tr span.hodnota"
        };

        final String[] currencySelectors = new String[]{
                "tr:has(span:containsOwn(Začiatočná predpokladaná celková hodnota zákazky)) + tr span.hodnota" +
                        " + span.hodnota",
                "tr:has(span:containsOwn(INFORMÁCIE O HODNOTE ZÁKAZKY)) + tr:not(:has(span)) + tr span.hodnota" +
                        " + span.hodnota"
        };

        return parsePrice(lot, false, priceSelectors, currencySelectors, null);
    }

    /**
     * Create element for each lot in OZZ form.
     *
     * @param document element to be parsed from
     * @return List<Element>
     */
    private static List<Element> getOzzLotsSubsections(final Element document) {
        Element root = document.select("span.nadpis:matchesOwn(ODDIEL V(:|.)) + table table > tbody").first();

        if (root == null) {
            return null;
        }

        List<Element> lotFirstLines = root.select(
                "tr:not(:has(table)):has(td:has(span:containsOwn(mluva k)))");

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
