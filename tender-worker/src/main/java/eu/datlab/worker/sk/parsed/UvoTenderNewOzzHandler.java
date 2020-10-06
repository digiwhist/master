package eu.datlab.worker.sk.parsed;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Tag;
import org.jsoup.select.Elements;

import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getFirstValueFromElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getTrueOrFalseFromElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.parseLotNumber;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.parsePrice;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.removeFakeLots;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Stream;

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
     * Merges two sets of lots.
     *
     * @param lotsA first set of lots
     * @param lotsB second set of lots
     * @return non-empty result of merging or null
     */
    private static List<Element> mergeLots(final List<Element> lotsA, final List<Element> lotsB) {
        if (lotsA == null) {
            return lotsB;
        }
        if (lotsB == null) {
            return lotsA;
        }

        // details which are shared with all lots (parts without lot number)
        Element share = new Element(Tag.valueOf("wrapper"), "");
        Stream.concat(lotsA.stream(), lotsB.stream())
                .filter(n -> parseLotNumber(n) == null)
                .forEach(n -> share.append(n.html()));

        // merging of lots with same number
        List<Element> lots = new ArrayList<>();
        Stream.concat(lotsA.stream(), lotsB.stream())
                .filter(n -> parseLotNumber(n) != null)
                .forEach(n -> {
                    Element existingLot = lots.stream().filter(l -> parseLotNumber(n).equals(parseLotNumber(l))).findFirst().orElse(null);
                    if (existingLot == null) {
                        lots.add(n);
                    } else {
                        existingLot.append(n.html());
                    }
                });

        // append shared details
        if (!share.children().isEmpty()) {
            if (lots.isEmpty()) {
                lots.add(share);
            } else {
                lots.forEach(n -> n.append(share.html()));
            }
        }

        return lots.isEmpty() ? null : lots;
    }

    /**
     * Parse tender lots from element.
     *
     * @param document element to parse from
     * @return List<ParsedBasicTenderLot> or null
     */
    private static List<ParsedTenderLot> parseLots(final Document document) {
        List<Element> lotNodes = new ArrayList<>();

        Element partV = JsoupUtils.selectFirst("fieldset:has(legend:containsOwn(ODDIEL V:))", document);

        lotNodes = mergeLots(getOzzLotsSubsections(document, "II"), getOzzLotsSubsections(document, "V"));

        if (lotNodes == null || lotNodes.isEmpty()) {
            return null;
        }

        final List<ParsedTenderLot> parsedLots = new ArrayList<>();

        int positionOnPage = 1;
        for (Element lot : lotNodes) {
            Element cancellationNode = JsoupUtils.selectFirst("span.title:containsOwn(Zmluva/časť nebola pridelená)", lot);
            String cancellationReason = null;
            if (cancellationNode != null) {
                if (cancellationNode.parent().className().equals("selectList")) {
                    cancellationReason = cancellationNode.nextElementSibling().text();
                } else {
                    cancellationReason = cancellationNode.parent().nextElementSibling().text();
                }
            }

            parsedLots.add(new ParsedTenderLot()
                    .setPositionOnPage(String.valueOf(positionOnPage++))
                    .setLotNumber(parseLotNumber(lot))
                    .setTitle(getFirstValueFromElement(lot, "div:containsOwn(Názov:) > span"))
                    .setContractSignatureDate(getFirstValueFromElement(lot, "div:has(span:containsOwn(Dátum uzatvorenia zmluvy)) + div"))
                    .setBidsCount(parseBidsCount(lot, partV))
                    .setSmeBidsCount(parseSmeBidsCount(lot, partV))
                    .setOtherEuMemberStatesCompaniesBidsCount(parseByLabel("ponúk doručených od uchádzačov z iných členských", lot, partV))
                    .setElectronicBidsCount(parseByLabel("ponúk prijatých elektronicky", lot, partV))
                    .setNonEuMemberStatesCompaniesBidsCount(parseNonEuBidsCount(lot, partV))
                    .addBid(parsedBid(lot))
                    .setIsAwarded(parseIsLotAwarded(lot))
                    .setEstimatedPrice(parsePrice(lot,
                            "div:containsOwn(Hodnota):matches(odhadovaná|predpokladaná) > span + span + span",
                            "div:containsOwn(Hodnota):matches(odhadovaná|predpokladaná) > span",
                            "div:containsOwn(Hodnota):matches(odhadovaná|predpokladaná) > span + span", null))
                    .setCancellationReason(cancellationReason)
                    .setStatus(cancellationReason != null ? TenderLotStatus.CANCELLED.name() : null)
                    .setCpvs(parseCpvs(lot)));
        }

        return parsedLots.isEmpty() ? null : parsedLots;
    }

    /**
     * @param lot lot to be parsed
     * @return non-empty List<ParsedCpv> or Null
     */
    private static List<ParsedCPV> parseCpvs(final Element lot) {
        if (lot == null) {
            return null;
        }

        List<ParsedCPV> parsedCPVs = new ArrayList<>();

        String mainCpv = JsoupUtils.selectText("div.subtitle:has(span:containsOwn(Hlavný kód CPV)) + div", lot);
        if (mainCpv != null) {
            parsedCPVs.add(new ParsedCPV().setIsMain(Boolean.TRUE.toString()).setCode(mainCpv));
        }

        Element node = JsoupUtils.selectFirst("div.subtitle:has(span:containsOwn(Dodatočné kódy CPV))", lot);
        if (node != null) {
            do {
                node = node.nextElementSibling();
                if (node != null && node.className().equals("selectList")) {
                    parsedCPVs.add(new ParsedCPV().setIsMain(Boolean.FALSE.toString()).setCode(node.text()));
                }
            } while (node != null && !node.className().equals("subtitle"));
        }

        parsedCPVs.forEach(n -> n.setCode(n.getCode().trim().replaceAll("\\.+$", "")));

        return parsedCPVs.isEmpty() ? null : parsedCPVs;
    }

    /**
     * Parse non EU bids count.
     *
     * @param lot    lot to parse from
     * @param tender tender, context of the lot
     * @return String or null
     */
    private static String parseNonEuBidsCount(final Element lot, final Element tender) {
        String bidCount = parseByLabel("ponúk doručených od uchádzačov z iných členských", lot, tender);
        if (bidCount == null) {
            bidCount = parseByLabel("Počet uchádzačov z iných krajín", lot, tender);
        }

        return bidCount;
    }

    /**
     * Parse SME bids count.
     *
     * @param lot    lot to parse from
     * @param tender tender, context of the lot
     * @return String or null
     */
    private static String parseSmeBidsCount(final Element lot, final Element tender) {
        String bidCount = parseByLabel("ponúk doručených od malých", lot, tender);
        if (bidCount == null) {
            bidCount = parseByLabel("Počet zúčastnených malých a stredných podnikov", lot, tender);
        }

        return bidCount;
    }

    /**
     * Parse bid count.
     *
     * @param lot    lot to parse from
     * @param tender tender, context of the lot
     * @return String or null
     */
    private static String parseBidsCount(final Element lot, final Element tender) {
        String bidCount = parseByLabel("Počet prijatých ponúk:", lot, tender);
        if (bidCount == null) {
            bidCount = parseByLabel("Počet uchádzačov, ktorí sa budú zvažovať:", lot, tender);
        }

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
     * Parses value with the given label.
     *
     * @param label   label of the count field
     * @param context context to be searched
     * @return count or null
     */
    private static String parseByLabel(final String label, final Element context) {
        Element node = JsoupUtils.selectFirst("*:containsOwn(" + label + ")", context);
        if (node == null) {
            return null;
        }

        switch (node.tagName().toLowerCase()) {
            case "div":
                return JsoupUtils.selectText(":root > span", node);
            case "span":
                return JsoupUtils.selectText(":root + span", node);
            default:
                return null;
        }
    }

    /**
     * Parses value with the given label from the set of contexts. First not null value wins.
     *
     * @param label   label of the count field
     * @param context set of contexts to be parsed
     * @return count or null
     */
    private static String parseByLabel(final String label, final Element... context) {
        for (Element c : context) {
            String count = parseByLabel(label, c);
            if (count != null) {
                return count;
            }
        }

        return null;
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
            .setBidders(parseBidders(lot))
            .setIsSubcontracted(getFirstValueFromElement(lot, "div:containsOwn(predpoklad subdodávok) > span"))
            .setIsWinning(String.valueOf(true))
            .setPrice(parsePrice(lot,
                new String[]{
                    "div:containsOwn(Hodnota (ktorá sa brala do úvahy)) > span + span + span",
                    "div:containsOwn(ktorá sa brala do úvahy) > span + span + span",
                    "div:containsOwn(Celková hodnota zákazky/časti):not(:matches(odhadovaná|predpokladaná|edna)) + div > span + span" +
                        " + span",
                    "div:has(span:containsOwn(Hodnota ocenení)) + div > span + span + span"
                },
                new String[]{
                    "div:containsOwn(Hodnota (ktorá sa brala do úvahy)) > span",
                    "div:containsOwn(ktorá sa brala do úvahy) > span",
                    "div:containsOwn(Celková hodnota zákazky/časti):not(:matches(odhadovaná|predpokladaná|edna)) + div > span",
                    "div:has(span:containsOwn(Hodnota ocenení)) + div > span"
                },
                new String[]{
                    "div:containsOwn(Hodnota (ktorá sa brala do úvahy)) > span + span",
                    "div:containsOwn(ktorá sa brala do úvahy) > span + span",
                    "div:containsOwn(Celková hodnota zákazky/časti):not(:matches(odhadovaná|predpokladaná|edna)) + div > span + span",
                    "div:has(span:containsOwn(Hodnota ocenení)) + div > span + span"
                },
                null));

        if (parsedBid.getBidders() == null && parsedBid.getPrice() == null) {
            return null;
        } else {
            return parsedBid;
        }
    }

    /**
     * Parses lot bidders.
     *
     * @param lot to be parsed
     * @return list of ParsedBody or null
     */
    private static List<ParsedBody> parseBidders(final Element lot) {
        Elements nodes = JsoupUtils.select("div.contactSelectList", lot);

        List<ParsedBody> bidders = new ArrayList<>();
        for (Element n : nodes) {
            ParsedBody parsedBody = new ParsedBody()
                    .setName(getFirstValueFromElement(n, new String[]{
                            "div.contactSelectList > span.titleValue > span",
                            "div.contactSelectList > span.titleValue"}))
                    .setAddress(parseRawAddress(n))
                    .setIsSme(getTrueOrFalseFromElement(n.nextElementSibling(), "div:containsOwn(Dodávateľom je MS) > span"))
                    .setPhone(getFirstValueFromElement(n, "span:containsOwn(Telefón:) + span"))
                    .setEmail(getFirstValueFromElement(n, "span:containsOwn(Email) + span"));

            String bodyId = getFirstValueFromElement(n, new String[]{
                    "span:containsOwn(IČO:) + span",
                    "span:containsOwn(útroštátne identifikačné číslo:) + span"});

            if (bodyId != null) {
                parsedBody.setBodyIds(Arrays.asList(new BodyIdentifier()
                        .setId(bodyId)
                        .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                        .setScope(BodyIdentifier.Scope.SK)));
            }

            bidders.add(parsedBody);
        }

        return bidders.isEmpty() ? null : bidders;
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
     * @param document      element to be parsed from
     * @param sectionNumber number of section (for lots only 'II' and 'V' make sense)
     * @return List<Element>
     */
    private static List<Element> getOzzLotsSubsections(final Element document, final String sectionNumber) {
        Element root = document.select("fieldset:has(legend:matchesOwn(ODDIEL " + sectionNumber + "(:|.)))").first();

        if (root == null) {
            root = document.select("legend:matchesOwn(ODDIEL " + sectionNumber + "(:|.)) + div").first();
        }

        if (root == null) {
            return null;
        }

        removeFakeLots(document);
        List<Element> lotFirstLines = JsoupUtils.select(root, "span.title ~ span:matchesOwn(Časť: ?\\d+)",
                "div:has(span:matchesOwn(Časť: ?\\d+))", "fieldset > span:matchesOwn(Časť: ?\\d+)");

        if (lotFirstLines == null || lotFirstLines.isEmpty()) {
            lotFirstLines = root.select("span:matchesOwn(Časť: ?\\d+)");
        }

        if (lotFirstLines == null || lotFirstLines.isEmpty()) {
            return Arrays.asList(root);
        }

        Element sharedLotData = ParserUtils.getSubsectionOfElements(lotFirstLines.get(0).firstElementSibling(), lotFirstLines.get(0));

        if (sectionNumber.equals("II")) {
            return parseSectionII(lotFirstLines, sharedLotData);
        } else if (sectionNumber.equals("V")) {
            boolean hasAltLotDivider = !root.select("div:containsOwn(Zákazka č.:)").isEmpty();

            return parseSectionV(lotFirstLines, sharedLotData, hasAltLotDivider
                ? lotNode -> !lotNode.select("div:containsOwn(Zákazka č.:)").isEmpty()
                : null
            );
        } else {
            return null;
        }
    }

    /**
     * Parses section number II.
     *
     * @param lotFirstLines - list of first lines of each lot.
     * @param sharedLotData - shared data for all lots.
     * @return - list of elements containing data for each lot separately.
     */
    private static List<Element> parseSectionII(final List<Element> lotFirstLines, final Element sharedLotData) {
        List<Element> subsections = new ArrayList<>();
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
        // Parsing sections (every section matches one single lot).
        for (int iterator = 0; iterator < lotFirstLines.size(); iterator++) {
            Element lotData;
            if ((iterator + 1) != lotFirstLines.size()) {
                lotData = ParserUtils.getSubsectionOfElements(lotFirstLines.get(iterator), lotFirstLines.get(iterator + 1));
            } else {
                lotData = ParserUtils.getSubsectionOfElements(lotFirstLines.get(iterator), null);
            }
            subsections.add(sharedLotData.clone().append(lotData.html()));
        }
        return subsections;
    }

    /**
     * Parses section number V.
     *
     * @param lotFirstLines - list of lines containing "Čásť:".
     * @param sharedLotData - shared data for all lots.
     * @param makeNewPart - if it is not null controls creating of new subsections, otherwise adds each sub-part as separate section
     * @return - list of elements containing data for each lot separately.
     */
    private static List<Element> parseSectionV(final List<Element> lotFirstLines, final Element sharedLotData,
                                               final Predicate<Element> makeNewPart) {
        List<Element> subsections = new ArrayList<>();
        Element lotData = null;
        Element partOfLotData;
        for (int iterator = 0; iterator < lotFirstLines.size(); iterator++) {
            if ((iterator + 1) != lotFirstLines.size()) {
                partOfLotData = ParserUtils.getSubsectionOfElements(lotFirstLines.get(iterator), lotFirstLines.get(iterator + 1));
            } else {
                partOfLotData = ParserUtils.getSubsectionOfElements(lotFirstLines.get(iterator), null);
            }

            if (iterator == 0 || (makeNewPart != null && makeNewPart.negate().test(partOfLotData))) {
                if (lotData == null) {
                    lotData = partOfLotData;
                } else {
                    lotData.append(partOfLotData.html());
                }
            } else { // The first lot starts here, so we add data about the previous section to the output list.
                subsections.add(sharedLotData.clone().append(lotData.html()));
                lotData = partOfLotData;
            }
        }
        // adding the last section to the output list.
        if (lotData != null) {
            subsections.add(sharedLotData.clone().append(lotData.html()));
        }
        return subsections;
    }
}

