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
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getTrueOrFalseFromElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.parsePrice;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Parser for Uvo Tender old OZZ form specific data.
 *
 * @author Michal Riha
 */
final class UvoTenderNewOzzHandler {
    private static final String IN_PART_II = "legend:matchesOwn(ODDIEL II.*) + div ";

    /**
     * Private constructor to make this class static.
     */
    private UvoTenderNewOzzHandler() {
    }

    /**
     * Parse method for Uvo Tender OZZ form specific data.
     *
     * @param parsedTender tender to add data to
     * @param document     document to parse data from
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
            parsedLots.add(
                    new ParsedTenderLot()
                            .setPositionOnPage(String.valueOf(positionOnPage++))
                            .setLotNumber(parseLotNumber(lot))
                            .setTitle(getFirstValueFromElement(lot, "div:containsOwn(Názov:) > span"))
                            .setAwardDecisionDate(getFirstValueFromElement(lot, "div:has(span:containsOwn(Dátum uzatvorenia " +
                                    "zmluvy)) + div"))
                            .setBidsCount(parseBidCount(lot))
                            .setSmeBidsCount(getFirstValueFromElement(lot, "span:containsOwn(ponúk doručených od malých) + span"))
                            .setOtherEuMemberStatesCompaniesBidsCount(getFirstValueFromElement(lot, "div:containsOwn(ponúk do" +
                                    "ručených od uchádzačov z iných členských) > span"))
                            .setElectronicBidsCount(getFirstValueFromElement(lot, "div:containsOwn(elektronick) span"))
                            .setNonEuMemberStatesCompaniesBidsCount(getFirstValueFromElement(lot, "div:containsOwn(nie sú členm) span"))
                            .addBid(parsedBid(lot))
                            .setIsAwarded(parseIsLotAwarded(lot))
                            .setEstimatedPrice(parsePrice(lot,
                                    "div:containsOwn(Hodnota):matches(odhadovaná|predpokladaná) > span + span + span",
                                    "div:containsOwn(Hodnota):matches(odhadovaná|predpokladaná) > span",
                                    "div:containsOwn(Hodnota):matches(odhadovaná|predpokladaná) > span + span", null)));
        }

        return parsedLots.isEmpty() ? null : parsedLots;
    }

    /**
     * Parse bid count.
     *
     * @param lot lot to parse from
     * @return String or null
     */
    private static String parseBidCount(final Element lot) {
        String bidCount = getFirstValueFromElement(lot, "div:containsOwn(Počet prijatých ponúk:) > span");

        if (bidCount == null) {
            final String noBids = getFirstValueFromElement(lot, "div:has(span:containsOwn(Zmluva/časť nebola " +
                    "pridelená)) + div:has(span:containsOwn(Neboli prijaté žiadne ponuky))");
            if (noBids != null && !noBids.trim().isEmpty()) {
                bidCount = "0";
            }
        }

        return bidCount;
    }

    /**
     * Parse if lot is awarded.
     *
     * @param lot lot to parse
     * @return String or null
     */
    private static String parseIsLotAwarded(final Element lot) {
        final String isAwarded = getFirstValueFromElement(lot, "span:containsOwn(Zákazka/časť je pridelená) + span");
        if (isAwarded != null) {
            if (isAwarded.toLowerCase().contains("áno")) {
                return String.valueOf(true);
            } else if (isAwarded.toLowerCase().contains("nie")) {
                return String.valueOf(false);
            }
        }
        return null;
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
                .setIsSubcontracted(getFirstValueFromElement(lot, "div:containsOwn(predpoklad subdodávok) > span"))
                .setIsWinning(String.valueOf(true))
                .setPrice(parsePrice(lot,
                        new String[]{"div:containsOwn(Hodnota):not(:matches(odhadovaná|predpokladaná|edna)) > span + span + span",
                                "span:containsOwn(Celková hodnota zákazky/časti) + span + span + span",
                                "div:containsOwn(Celková hodnota zákazky/časti) + div > span + span + span"},
                        new String[]{"div:containsOwn(Hodnota):not(:matches(odhadovaná|predpokladaná|edna)) > span",
                                "span:containsOwn(Celková hodnota zákazky/časti) + span",
                                "div:containsOwn(Celková hodnota zákazky/časti) + div > span"},
                        new String[]{"div:containsOwn(Hodnota):not(:matches(odhadovaná|predpokladaná|edna)) > span + span",
                                "span:containsOwn(Celková hodnota zákazky/časti) + span + span",
                                "div:containsOwn(Celková hodnota zákazky/časti) + div > span + span"},
                        null));

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
                        "div.contactSelectList > span.titleValue > span",
                        "div.contactSelectList > span.titleValue"}))
                .setAddress(parseRawAddress(lot))
                .setIsSme(getTrueOrFalseFromElement(lot, "div:containsOwn(Dodávateľom je MS) > span"))
                .setPhone(getFirstValueFromElement(lot, "span:containsOwn(Telefón:) + " +
                        "span"))
                .setEmail(getFirstValueFromElement(lot, "span:containsOwn(Email) + span"));

        String bodyId = getFirstValueFromElement(lot, new String[]{
                "span:containsOwn(IČO:) + span",
                "span:containsOwn(útroštátne identifikačné číslo:) + " +
                        "span"});

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
     * Parse lot number or lot.
     *
     * @param lot lot to parse from
     * @return String or null
     */
    private static String parseLotNumber(final Element lot) {
        String lotNumber = getFirstValueFromElement(lot, "span:containsOwn(Časť:)");

        return lotNumber == null ? null : lotNumber.replace("Časť:", "");
    }

    /**
     * Parse raw address of bidder from document.
     *
     * @param lot lot to be parsed
     * @return String or Null
     */
    private static ParsedAddress parseRawAddress(final Element lot) {
        Element element = lot.selectFirst("div.ContactSelectList > span.titleValue > text");

        if (element == null) {
            element = lot.selectFirst("div.ContactSelectList > span.titleValue");
        }

        if (element == null) {
            return null;
        }

        Element nuts = lot.selectFirst("span:containsOwn(Kód NUTS:) + span");
        Element country = lot.selectFirst("span:containsOwn(Slovensko)");

        return nuts == null ? null : new ParsedAddress()
                .addNuts(nuts.ownText())
                .setCountry(country == null ? null : country.ownText())
                .setRawAddress(element.ownText());
    }

    /**
     * Parse final price value from document.
     *
     * @param document document to be parsed
     * @return String or Null
     */
    private static ParsedPrice parseFinalPrice(final Document document) {

        String[] priceVatSelector = new String[]{
                IN_PART_II + "div:has(span:containsOwn(Celková konečná hodnota zmluvy)) + div + div",
                IN_PART_II + "div:has(span:containsOwn(jedna hodnota)) + div > div + span + span",
                IN_PART_II + "div:has(span:containsOwn(Celková hodnota obstarávania)) + div > span + span + span"};

        String[] priceSelectors = new String[]{
                IN_PART_II + "div:has(span:containsOwn(Celková konečná hodnota zmluvy)) + div > span",
                IN_PART_II + "div:has(span:containsOwn(jedna hodnota)) + div > span",
                IN_PART_II + "div:has(span:containsOwn(Celková hodnota obstarávania)) + div > span"};

        String[] currencySelectors = new String[]{
                IN_PART_II + "div:has(span:containsOwn(Celková konečná hodnota zmluvy)) + div > span + span",
                IN_PART_II + "div:has(span:containsOwn(jedna hodnota)) + div > span + span",
                IN_PART_II + "div:has(span:containsOwn(Celková hodnota obstarávania)) + div > span + span"};

        return parsePrice(document, priceVatSelector, priceSelectors, currencySelectors, null);
    }

    /**
     * Create element for each lot in OZZ form.
     *
     * @param document element to be parsed from
     * @return List<Element>
     */
    private static List<Element> getOzzLotsSubsections(final Element document) {
        Element root = document.select("fieldset:has(legend:matchesOwn(ODDIEL V(:|.)))").first();

        if (root == null) {
            root = document.select("legend:matchesOwn(ODDIEL V(:|.)) + div").first();
        }

        if (root == null) {
            return null;
        }

        List<Element> lotFirstLines = root.select("div:has(span:containsOwn(Časť:)), fieldset > span:containsOwn(Časť:)");

        if (lotFirstLines == null || lotFirstLines.isEmpty()) {
            lotFirstLines = root.select("span:containsOwn(Časť:)");
        }

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
