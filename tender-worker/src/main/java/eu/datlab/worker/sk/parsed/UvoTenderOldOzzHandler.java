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

import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getFirstValueFromElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.parsePrice;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Parser for Uvo Tender old OZZ form specific data. Data downloaded from
 * https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
 *
 * @author Michal Riha
 */
final class UvoTenderOldOzzHandler {
    private static final String IN_PART_II = "legend:matchesOwn(ODDIEL II.*) + div ";

    /**
     * Private constructor to make this class static.
     */
    private UvoTenderOldOzzHandler() {
    }

    /**
     * Parse method for Uvo Tender OZZ form specific data.
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
                .setFinalPrice(parseFinalPrice(document))
                .setLots(parseLots(document));
    }

    /**
     * Parse tender lots from element.
     *
     * @param document
     *         element to parse from
     *
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
            parsedLots.add(
                    new ParsedTenderLot()
                            .setPositionOnPage(String.valueOf(positionOnPage++))
                            .setLotNumber(parseLotNumber(lot))
                            .setTitle(getFirstValueFromElement(lot, "span:containsOwn(Názov) + span"))
                            .setAwardDecisionDate(getFirstValueFromElement(lot, new String[]{
                                    "div:has(span:containsOwn(Dátum uzatvorenia zmluvy)) + div",
                                    "div:has(span:containsOwn(DÁTUM ROZHODNUTIA O ZADANÍ ZÁKAZ)) + div"}))
                            .setBidsCount(getFirstValueFromElement(lot, new String[]{
                                    "div:has(span:containsOwn(Počet predložených ponúk)) + div",
                                    "div:containsOwn(očet prijatých ponúk) span",
                                    "div:has(span:containsOwn(očet uchádzačov, ktorí predložil)) + div > span"}))
                            .setElectronicBidsCount(getFirstValueFromElement(lot,
                                    "div:containsOwn(ponúk prijatých elektronickou) span"))
                            .addBid(parsedBid(lot))
                            .setEstimatedPrice(parsePrice(lot,
                                    "div:has(span:containsOwn(predpokladaná celková hodnota zákazky)) + div + div > " +
                                            "span",
                                    "div:has(span:containsOwn(predpokladaná celková hodnota zákazky)) + div > span",
                                    "div:has(span:containsOwn(predpokladaná celková hodnota zákazky)) + div > span + " +
                                            "span", null)));
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
                .setIsWinning(String.valueOf(true))
                .setIsSubcontracted(getFirstValueFromElement(lot, new String[]{
                        "div:has(span:containsOwn(predpoklad subdodávok)) + div",
                        "div:containsOwn(predpoklad subdodávok) span"}))
                .setPrice(parseBidPrice(lot))
                .setMonthlyPriceMonthsCount(getFirstValueFromElement(lot,
                        "div:has(span:containsOwn(Počet mesiacov)) + div > span"));

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
                .setName(getFirstValueFromElement(lot, "span.titleValue > span"))
                .setPhone(getFirstValueFromElement(lot, "span:containsOwn(Telefón) + span"))
                .setEmail(getFirstValueFromElement(lot, "span:containsOwn(Email) + span"));

        if (parseRawAddress(lot) != null) {
            parsedBody.setAddress(parseRawAddress(lot)
                    .setCountry(getFirstValueFromElement(lot, "div.contactSelectList " + "> span.titleValue ~ span")));
        }

        final String bodyId = getFirstValueFromElement(lot, new String[]{
                "span:containsOwn(IČO) + span",
                "span:containsOwn(Vnútroštátne identifikačné číslo)" +
                        " + span"});

        if (bodyId != null) {
            parsedBody.setBodyIds(Arrays.asList(new BodyIdentifier()
                    .setId(bodyId)
                    .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                    .setScope(BodyIdentifier.Scope.SK)));
        }

        if (parsedBody.getName() == null && parsedBody.getAddress() == null && parsedBody.getPhone() == null
                && parsedBody.getEmail() == null && parsedBody.getBodyIds() == null) {
            return null;
        } else {
            return parsedBody;
        }
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
     * Parse lot price.
     *
     * @param lot
     *         lot to parse
     *
     * @return parsed price of lot
     */
    private static ParsedPrice parseBidPrice(final Element lot) {
        final String[] vatSelector = new String[]{
                "div:has(span:containsOwn(Celková konečná hodnota zákazky)) ~ div.shorttext_1e60f717fde90baf473fc" +
                        "7c874d5bfd535a18195ad8b2c762cbd8818fbc9076a + div > span",
                "div:has(span:containsOwn(Celková konečná hodnota zákazky)) + div + div > span",
                "div:has(span:containsOwn(nformácie o hodnote zmluvy)) + div + div > span"};

        final String[] priceSelector = new String[]{
                "div:has(span:containsOwn(Celková konečná hodnota zákazky)) ~ div.shorttext_1e60f717fde90baf473fc" +
                        "7c874d5bfd535a18195ad8b2c762cbd8818fbc9076a:not(:containsOwn(ajnižšia)) > span",
                "div:has(span:containsOwn(Celková konečná hodnota zákazky)) + div:not(:containsOwn(ajnižšia)) > span",
                "div:has(span:containsOwn(nformácie o hodnote zmluvy)) + div > span:not(:containsOwn(ačiatočná))"};

        final String[] currencySelector = new String[]{
                "div:has(span:containsOwn(Celková konečná hodnota zákazky)) ~ div.shorttext_1e60f717fde90baf473fc" +
                        "7c874d5bfd535a18195ad8b2c762cbd8818fbc9076a > span + span",
                "div:has(span:containsOwn(Celková konečná hodnota zákazky)) + div > span + span",
                "div:has(span:containsOwn(nformácie o hodnote zmluvy)) + div > span + span"};

        final String[] vatValueSelector = new String[]{
                "div:has(span:containsOwn(nformácie o hodnote zmluvy)) + div + div + div > span"};

        ParsedPrice result = parsePrice(lot, vatSelector, priceSelector, currencySelector, vatValueSelector);

        // try to parse min max price range
        if (result == null) {
            final String minPrice = getFirstValueFromElement(lot,
                    "div:has(span:containsOwn(Celková konečná hodnota zákazky)) + div:containsOwn(ajnižšia) > span");
            final String maxPrice = getFirstValueFromElement(lot,
                    "div:has(span:containsOwn(Celková konečná hodnota zákazky)) + div:containsOwn(ajvyššia) > span " +
                            "+ span");

            if (minPrice != null && maxPrice != null) {
                result = new ParsedPrice()
                        .setMinNetAmount(minPrice)
                        .setMaxNetAmount(maxPrice)
                        .setCurrency(getFirstValueFromElement(lot,
                                "div:has(span:containsOwn(Celková konečná hodnota zákazky)) + div:containsOwn(a" +
                                        "jvyššia) > span + span + span"));
            }
        }

        return result;
    }

    /**
     * Parse raw address of bidder from document.
     *
     * @param lot
     *         lot to be parsed
     *
     * @return String or Null
     */
    private static ParsedAddress parseRawAddress(final Element lot) {
        Element element = lot.select("span.titleValue").first();

        if (element != null) {
            String[] tempStrings = element.html().split("<br>");
            if (tempStrings.length > 2) {
                return new ParsedAddress().setRawAddress(tempStrings[2]);
            }
        }

        return null;
    }

    /**
     * Parse final price from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private static ParsedPrice parseFinalPrice(final Document document) {
        String[] priceVatSelector = new String[]{
                IN_PART_II + "div:has(span:containsOwn(Celková konečná hodnota zmluvy)) + div + div",
                IN_PART_II + "div:has(span:containsOwn(jedna hodnota)) + div > div + span + span",
                IN_PART_II + "div:has(span:containsOwn(lkové množstvo alebo rozs)) + div + div:has(span:containsOwn(" +
                        "bez DPH))",
                IN_PART_II + "div:has(span:containsOwn(jedna hodnota)) + div + div > span"};

        String[] priceSelectors = new String[]{
                IN_PART_II + "div:has(span:containsOwn(Celková konečná hodnota zmluvy)) + div:not(:containsOwn" +
                        "(Najnižšia)) > span",
                IN_PART_II + "div:has(span:containsOwn(jedna hodnota)) + div > span",
                IN_PART_II + "div:has(span:containsOwn(Predpokladaná hodnota zákazky)) + div:containsOwn(Hodnota) >" +
                        " span"};

        String[] currencySelectors = new String[]{
                IN_PART_II + "div:has(span:containsOwn(Celková konečná hodnota zmluvy)) + div > span + span",
                IN_PART_II + "div:has(span:containsOwn(jedna hodnota)) + div > span + span",
                IN_PART_II + "div:has(span:containsOwn(lkové množstvo alebo rozs)) + div + div:has(span:containsOwn(" +
                        "bez DPH)) + div > span + span"};

        String[] vatAmountSelectors = new String[]{
                IN_PART_II + "div:has(span:containsOwn(jedna hodnota)) + div + div + div:containsOwn(Sadzba DP) > " +
                        "span"};


        ParsedPrice result =
                parsePrice(document, priceVatSelector, priceSelectors, currencySelectors, vatAmountSelectors);

        // try to parse min max price range
        if (result == null) {
            final String minPrice = getFirstValueFromElement(document,
                    IN_PART_II + "div:has(span:containsOwn(Celková konečná hodnota zmluvy)) + div:conta" +
                            "insOwn(Najnižšia) > span");
            final String maxPrice = getFirstValueFromElement(document,
                    IN_PART_II + "div:has(span:containsOwn(Celková konečná hodnota zmluvy)) + div:conta" +
                            "insOwn(Najvyšši) > span + span");

            if (minPrice != null && maxPrice != null) {
                result = new ParsedPrice()
                        .setMinNetAmount(minPrice)
                        .setMaxNetAmount(maxPrice)
                        .setCurrency(getFirstValueFromElement(document,
                                IN_PART_II + "div:containsOwn(Hodnota):matches(Od) > span + span + span"));
            }
        }

        return result;
    }

    /**
     * Create element for each lot in OZZ form.
     *
     * @param document
     *         element to be parsed from
     *
     * @return List<Element>
     */
    private static List<Element> getOzzLotsSubsections(final Element document) {
        Element root = document.select("legend:matchesOwn(ODDIEL V(.|:)) + div").first();

        if (root == null) {
            return null;
        }

        List<Element> lotFirstLines = root.select("span:containsOwn(Časť:):not(:contains(Lokalita))");

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
