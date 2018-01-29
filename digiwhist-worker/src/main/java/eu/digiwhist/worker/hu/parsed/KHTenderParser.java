package eu.digiwhist.worker.hu.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Tender parsed for KH.
 *
 * @author Marek Mikes
 */
public class KHTenderParser extends BaseDigiwhistTenderParser {
    private static final String VERSION = "1";

    private static final String FIRST_SECTION_START = "div > div:contains(I. szakasz:)";
    private static final String SECOND_SECTION_START = "div > div:matches((?i)II(\\.|\\.A) SZAKASZ(:| :))";
    private static final String THIRD_SECTION_START = "div > div:matches((?i)III(\\.|\\.A) SZAKASZ(:| :))";
    private static final String FOURTH_SECTION_START = "div > div:matches((?i)IV(\\.|\\.A) SZAKASZ(:| :))";
    private static final String LOT_SECTION_START = "div > div > div:contains(: Az eljárás eredménye)";
    private static final String ANY_SECTION_AFTER = " ~ div:matches(^(I|V)(\\.|I|V)(\\.\\ |\\ ).*)";
    private static final String ANY_SUBSECTION_AFTER = " ~ div:matches(^(I|V)(\\.|I|V).*)";

    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        final Document form = Jsoup.parse(rawTender.getSourceData());

        // parse from from overview
        ParsedTender parsedTender = new ParsedTender()
                .setSupplyType(getTdUnderTh("Beszerzés tárgya:", form))
                .setProcedureType(getTdUnderTh("Eljárás fajtája:", form))
                .setBidDeadline(getTdUnderTh("Ajánlattételi/részvételi jelentkezési határidő:", form))
                .addCpv(new ParsedCPV()
                        .setCode(getTdUnderTh("CPV Kód:", form))
                        .setIsMain(Boolean.TRUE.toString()))
                .addPublication(new ParsedPublication()
                        .setIsIncluded(true)
                        .setSource(PublicationSources.HU_KH)
                        .setSourceId(getTdUnderTh("Iktatószám:", form))
                        .setHumanReadableUrl(rawTender.getSourceUrl().toString())
                        .setPublicationDate(getTdUnderTh("Közzététel dátuma:", form))
                        .setSourceFormType(getTdUnderTh("Hirdetmény típusa:", form)));

        Element document = form.select("div.content.hirdetmenytartalom > div").first();

        if (document == null) {
            document = form.select("div.notice > div").first();
        }

        Element part1 = document.select(FIRST_SECTION_START).first();
        if (part1 != null && part1.childNodes().size() < 20) {
            part1 = ParserUtils.getSubsectionOfElements(
                    document.select(FIRST_SECTION_START).first(),
                    document.select(FIRST_SECTION_START + ANY_SECTION_AFTER).first().previousElementSibling());
        }

        Element part2 = document.select(SECOND_SECTION_START).first();
        if (part2 != null && part2.childNodes().size() < 20) {
            part2 = ParserUtils.getSubsectionOfElements(
                    document.select(SECOND_SECTION_START).first(),
                    document.select(SECOND_SECTION_START + ANY_SECTION_AFTER).first().previousElementSibling());
        }

        Element part3 = document.select(THIRD_SECTION_START).first();
        if (part3 != null && part3.childNodes().size() < 20) {
            part3 = ParserUtils.getSubsectionOfElements(
                    document.select(THIRD_SECTION_START).first(),
                    document.select(THIRD_SECTION_START + ANY_SECTION_AFTER).first().previousElementSibling());
        }

        Element part4 = document.select(FOURTH_SECTION_START).first();
        if (part4 != null && part4.childNodes().size() < 20) {
            part4 = ParserUtils.getSubsectionOfElements(
                    document.select(FOURTH_SECTION_START).first(),
                    document.select(FOURTH_SECTION_START + ANY_SECTION_AFTER).first().previousElementSibling());
        }

        Element partLots = ParserUtils.getSubsectionOfElements(
                document.select(LOT_SECTION_START).first(),
                document.select(LOT_SECTION_START + ANY_SUBSECTION_AFTER).first());

        if (partLots == null) {
            partLots = part2;
        }

        String buyerName = getValueMultipleWays("Hivatalos név", part1);
        if (buyerName == null) {
            buyerName = getTdUnderTh("Ajánlatkérő", form);
        }

        parsedTender
                .addBuyer(new ParsedBody()
                        .setName(buyerName)
                        .addBodyId(new BodyIdentifier()
                                .setId(getValueMultipleWays("Nemzeti azonosító", part1))
                                .setType(BodyIdentifier.Type.HEADER_ICO)
                                .setScope(BodyIdentifier.Scope.HU))
                        .setAddress(new ParsedAddress()
                                .setStreet(getValueMultipleWays("Postai cím", part1))
                                .setCity(getValueMultipleWays("Város", part1))
                                .setPostcode(getValueMultipleWays("Postai irányítószám", part1))
                                .setCountry(getValueMultipleWays("Ország", part1))
                                .setUrl(getValueMultipleWays("Az ajánlatkérő általános címe (URL)", part1))
                                .addNuts(getValueMultipleWays("NUTS", part1)))
                        .setContactName(getValueMultipleWays(new String[]{
                                "Kapcsolattartó személy",
                                "Címzett"
                        }, part1))
                        .setPhone(getValueMultipleWays("Telefon", part1))
                        .setEmail(getValueMultipleWays("E-mail", part1))
                        .setBuyerType(getTdUnderTh("Ajánlatkérő típusa:", form))
                        .addMainActivity(getTdUnderTh("Ajánlatkérő fő tevényeségi köre:", form)))
                .setIsOnBehalfOf(
                        getValueMultipleWays("Az ajánlatkérő más ajánlatkérők nevében végzi a beszerzést", part1))
                .setTitle(getValueMultipleWays("Elnevezés", part2))
                .setDocumentsDeadline(JsoupUtils.selectText("div:contains(A dokumentáció beszerzésének határideje) +" +
                        " div div:containsOwn(Dátum:) > span", part4))
                .setAddressOfImplementation(new ParsedAddress()
                        .setRawAddress(getDivUnderDiv("A teljesítés helye:", part2))
                        .addNuts(getValueMultipleWays("NUTS-kód", part2)))
                .setDescription(getValueMultipleWays(new String[]{
                        "A szerződés vagy a beszerzés(ek) rövid meghatározá",
                        "II.1.4\\) Rövid meghatározás:"}, part2))
                .setCpvs(parseCpvs(part2))
                .setHasLots(getValueMultipleWays("A beszerzés részekből áll", part2))
                .setHasOptions(getValueMultipleWays("Opció", part2))
                .setAreVariantsAccepted(getValueMultipleWays("Elfogadhatók változatok \\(alternatív aj", part2))
                .setFinalPrice(parseFinalPrice(part2))
                .setBuyerAssignedId(getValueMultipleWays("Hivatkozási szám", part2))
                .setAwardCriteria(parseAwardCriteria(document))
                .addPublication(parsePreviousPublication(part3))
                .addPublication(parseTedPublication(part3))
                .setPersonalRequirements(JsoupUtils.selectText(
                        "div:contains(ajánlattevő/részvételre jelentkező személyes helyzeté) + div + div", part3))
                .setEconomicRequirements(
                        JsoupUtils.selectText("div:contains(Gazdasági és pénzügyi alkalmas) + div + div", part3))
                .setIsCoveredByGpa(getValueMultipleWays("özbeszerzési megállapodás (GPA) hatá", document))
                .setTechnicalRequirements(
                        JsoupUtils.selectText("div:contains(szaki, illetve szakmai alkalmass) + div + div", part3))
                .addEligibleBidLanguage(
                        JsoupUtils.selectText("div div:has(div:containsOwn(IV.2.4\\) Azok a nyelvek, amelyeken)) + div",
                                part4))
                .setEstimatedPrice(new ParsedPrice()
                        .setNetAmount(getValueMultipleWays("Érték áfa nélkül:", part2))
                        .setCurrency(JsoupUtils.selectText("div:containsOwn(Érték áfa nélkül:) span + span", part2)))
                .setSelectionMethod(
                        getCheckedValue("IV.2.1\\) Bírálati szempontok", "IV.2.2\\) Elektronikus árlejtésre vona",
                                part4))
                .setIsElectronicAuction(getValueMultipleWays("Elektronikus árlejtést alkalmaztak", part4))
                .setLots(parseLots(partLots));

        parsedTender = additionalParsing(parsedTender, part1, part2, part4);

        return new ArrayList<>(Collections.singletonList(parsedTender));
    }

    /**
     * Additional parsing.
     *
     * @param parsedTender parsed tender
     * @param part1 part from which to parse
     * @param part2 part from which to parse
     * @param part4 part from which to parse
     *
     * @return parsedTender
     */
    private ParsedTender additionalParsing(final ParsedTender parsedTender, final Element part1, final Element part2,
                                           final Element part4) {

        // below is additional parsing of stuff in unusual places
        if (parsedTender.getTitle() == null) {
            parsedTender.setTitle(getDivUnderDiv("elnevezés", part2));
        }

        if (parsedTender.getPublications().get(0).getSourceFormType() == null) {
            parsedTender.getPublications().get(0).setSourceFormType(JsoupUtils.selectText("b:containsOwn(irdetmény " +
                    "kizárólag előzetes tájékoztatás céljára szol)", part1));
        }

        if (parsedTender.getBuyers().get(0).getBuyerType() == null) {
            parsedTender.getBuyers().get(0).setBuyerType(parseBuyerType(part1));
        }

        if (parsedTender.getLots() == null) {
            parsedTender.addLot(new ParsedTenderLot()
                    .setTitle(parsedTender.getTitle())
                    .setLotNumber("1")
                    .setCpvs(parsedTender.getCpvs())
                    .setEstimatedPrice(parsedTender.getEstimatedPrice()));
        }

        if (parsedTender.getProcedureType() == null) {
            parsedTender.setProcedureType(getCheckedValue("IV.1.1\\) Az eljárás fajtája",
                    "IV.2\\) Bírálati szempontok", part4));
        }

        if (parsedTender.getNationalProcedureType() == null) {
            parsedTender.setNationalProcedureType(parsedTender.getProcedureType());
        }

        if (parsedTender.getSelectionMethod() == null) {
            parsedTender.setSelectionMethod(getCheckedValue("Értékelési szempontok", "Opciókra vonatkozó informáci",
                    part2));
        }

        if (parsedTender.getEligibleBidLanguages() == null || parsedTender.getEligibleBidLanguages().isEmpty()) {
            final String bidLanguages = getSecondValueMultipleWays("Az EU következő hivatalos nyelve", part4);

            if (bidLanguages != null) {
                for (String bidLanguage : bidLanguages.trim().split(" ")) {
                    parsedTender.addEligibleBidLanguage(bidLanguage);
                }
            }
        }

        return parsedTender;
    }


    /**
     * Parse checked buyer type.
     *
     * @param part1 part from which to parse
     *
     * @return String or null
     */
    private String parseBuyerType(final Element part1) {
        final Element partBuyerType = ParserUtils.getSubsectionOfElements(
                part1.select("div > div:has(div:containsOwn(I.4\\) Az ajánlatkérő típusa))").first(),
                part1.select("div > div:has(div:containsOwn(I.5\\) Fő tevékenység))").first());

        return JsoupUtils.selectText("span:has(b:containsOwn(x)) + span", partBuyerType);
    }

    /**
     * Parse previous publication.
     *
     * @param part3 document to parse from
     *
     * @return ParsedPublication or null
     */
    private ParsedPublication parsePreviousPublication(final Element part3) {
        final Elements previousPublicationid = JsoupUtils.select("*:containsOwn(A hirdetmény száma a Közbeszerzési " +
                "Értesítőben:) > span", part3);

        if (previousPublicationid == null || previousPublicationid.isEmpty()) {
            return null;
        }

        for (int i = 0; previousPublicationid.size() > i; i++) {
            if (!previousPublicationid.get(i).text().trim().isEmpty() && previousPublicationid.size() > i + 1) {
                return new ParsedPublication()
                        .setSourceId(previousPublicationid.get(i).text().trim() + "/" + previousPublicationid.get(i + 1)
                                .text().trim())
                        .setSource(PublicationSources.HU_KH)
                        .setIsIncluded(false);
            }
        }

        return null;
    }

    /**
     * Parse ted publication.
     *
     * @param part3 document to parse from
     *
     * @return ParsedPublication or null
     */
    private ParsedPublication parseTedPublication(final Element part3) {
        final Elements previousPublicationid = JsoupUtils.select(
                "*:containsOwn(A hirdetmény száma a Hivatalos Lapban:) > span", part3);

        if (previousPublicationid == null || previousPublicationid.isEmpty()) {
            return null;
        }

        for (int i = 0; previousPublicationid.size() > i; i++) {
            if (!previousPublicationid.get(i).text().trim().isEmpty() && previousPublicationid.size() > i + 1) {
                return new ParsedPublication()
                        .setSourceId(previousPublicationid.get(i).text().trim() + "/" + previousPublicationid.get(i + 1)
                                .text().trim())
                        .setSource(PublicationSources.EU_TED)
                        .setIsIncluded(false);
            }
        }

        return null;
    }

    /**
     * Parse award criteria.
     *
     * @param element data to parse from
     *
     * @return List<ParsedAwardCriterion> or null
     */
    private List<ParsedAwardCriterion> parseAwardCriteria(final Element element) {
        Elements criteria = JsoupUtils.select("div:contains(Az összességében legelőnyösebb ajánlat a következő" +
                " részszempontok alapján) + div tbody > tr", element);

        if (criteria.isEmpty()) {
            criteria = JsoupUtils.select("tbody:has(th:containsOwn(Szempont)) > tr", element);
        }

        final List<ParsedAwardCriterion> result = new ArrayList<>();

        if (criteria.isEmpty()) {
            Element criteriaPart = ParserUtils.getSubsectionOfElements(
                    element.select(
                            "span > div > div > div:has(div:containsOwn(II.2.5\\) Értékelési szempontok))").first(),
                    element.select("span > div > div > div:has(div:containsOwn(II.2.11\\) Opciókra vonatkozó inform)" +
                            ")").first());

            if (criteriaPart != null) {
                result.add(new ParsedAwardCriterion()
                        .setName(JsoupUtils.selectOwnText("div:containsOwn(Minőségi kritérium) > span + span",
                                criteriaPart))
                        .setWeight(getValueMultipleWays("Minőségi kritérium", criteriaPart)));

                result.add(new ParsedAwardCriterion()
                        .setName(JsoupUtils.selectOwnText("div:containsOwn(Ár – Súlys)", criteriaPart))
                        .setWeight(
                                JsoupUtils.selectOwnText("div:containsOwn(Ár – Súlys) > span + span", criteriaPart)));
            }
        } else if (criteria.size() > 1) {
            for (int iterator = 1; criteria.size() > iterator; iterator++) {
                result.add(new ParsedAwardCriterion()
                        .setName(JsoupUtils.selectText("td", criteria.get(iterator)))
                        .setWeight(JsoupUtils.selectText("td + td", criteria.get(iterator))));
            }
        }

        return result.isEmpty() ? null : result;
    }

    /**
     * Parse lots.
     *
     * @param partLots element to parse from
     *
     * @return List<ParsedTenderLot> or null
     */
    private List<ParsedTenderLot> parseLots(final Element partLots) {
        if (partLots == null) {
            return null;
        }

        final Elements lotFirstLines = partLots.select("div > div:has(div > div:contains(A szerződés száma))");

        final Elements lotsDivided = new Elements();

        // No lots to parse found
        if (lotFirstLines == null || lotFirstLines.isEmpty()) {
            return null;

            // more lots to parse, divide them into elements and parse one by one
        } else {
            for (int iterator = 0; iterator < lotFirstLines.size(); iterator++) {
                if ((iterator + 1) != lotFirstLines.size()) {
                    lotsDivided.add(ParserUtils.getSubsectionOfElements(lotFirstLines.get(iterator),
                            lotFirstLines.get(iterator + 1)));
                } else {
                    lotsDivided.add(ParserUtils.getSubsectionOfElements(lotFirstLines.get(iterator), null));
                }
            }
        }

        final List<ParsedTenderLot> result = new ArrayList<>();

        int positionOnPage = 1;
        for (Element lot : lotsDivided) {
            ParsedTenderLot parsedTenderLot = new ParsedTenderLot()
                    .setPositionOnPage(String.valueOf(positionOnPage++))
                    .setContractNumber(getValueMultipleWays("A szerződés száma:", lot))
                    .setLotNumber(getSecondValueMultipleWays("Rész száma:", lot))
                    .setTitle(JsoupUtils.selectText("div > div:contains(A szerződés száma:) span + span" +
                            " + span", lot))
                    .setIsAwarded(getValueMultipleWays("zerződés/rész odaítélésre ker", lot))
                    .setAwardDecisionDate(getValueMultipleWays("A szerződés megkötésének dátuma:", lot))
                    .setBidsCount(getValueMultipleWays("A beérkezett ajánlatok szám", lot))
                    .setElectronicBidsCount(getValueMultipleWays("Elektronikus úton beérkezett ajánlatok száma", lot))
                    .setCpvs(parseCpvs(lot))
                    .setEstimatedPrice(new ParsedPrice()
                            .setNetAmount(getValueMultipleWays(new String[]{
                                    "A szerződés eredetileg becsült összé",
                                    "A szerződés/rész végleges összértéke:"}, lot))
                            .setCurrency(getSecondValueMultipleWays("A szerződés eredetileg becsült összé", lot)))
                    .addBid(new ParsedBid()
                            .setIsWinning(Boolean.TRUE.toString())
                            .setPrice(new ParsedPrice()
                                    .setNetAmount(getValueMultipleWays("A szerződés végleges összért", lot))
                                    .setCurrency(getSecondValueMultipleWays("A szerződés végleges összért", lot))
                            )
                            .setIsConsortium(
                                    getValueMultipleWays("A szerződést gazdasági szereplők csoportosulása nyer", lot))
                            .addBidder(new ParsedBody()
                                    .setName(getValueMultipleWays("Hivatalos név:", lot))
                                    .setAddress(new ParsedAddress()
                                            .setStreet(getValueMultipleWays("Postai cím:", lot))
                                            .setCity(getValueMultipleWays("Város:", lot))
                                            .setPostcode(getValueMultipleWays("ostai irányítószám:", lot))
                                            .setCountry(getValueMultipleWays("Ország:", lot))
                                            .addNuts(getValueMultipleWays("NUTS-kód", lot))
                                            .setUrl(getValueMultipleWays("Internetcím(ek)", lot)))
                                    .setEmail(getValueMultipleWays("E-mail:", lot))
                                    .setPhone(getValueMultipleWays("Telefon:", lot))));

            if (parsedTenderLot.getAwardDecisionDate() == null) {
                parsedTenderLot.setAwardDecisionDate(
                        getValueMultipleWays("Az eljárást lezáró döntés meghozatalának időpontja", lot));
            }

            if (parsedTenderLot.getEstimatedPrice().getCurrency() == null) {
                parsedTenderLot.getEstimatedPrice().setCurrency(getValueMultipleWays("Pénznem:", lot));
            }


            result.add(parsedTenderLot);
        }

        return result.isEmpty() ? null : result;
    }

    /**
     * Parse final price.
     *
     * @param part2 element to parse from
     *
     * @return ParsedPriceor null
     */
    private ParsedPrice parseFinalPrice(final Element part2) {
        final String price = getValueMultipleWays("Érték:", part2);

        if (price == null || price.trim().isEmpty()) {
            return null;
        }

        final String currency = getValueMultipleWays("Pénznem:", part2);
        final String netOrWithVAT = JsoupUtils.selectText("div:containsOwn(A beszerzés végleges összértéke)",
                part2);

        if (netOrWithVAT != null && netOrWithVAT.contains("Áfával")) {
            return new ParsedPrice()
                    .setAmountWithVat(price)
                    .setCurrency(currency);
        } else {
            return new ParsedPrice()
                    .setNetAmount(price)
                    .setCurrency(currency);
        }
    }

    /**
     * Parse cpvs.
     *
     * @param part2 element to parse from
     *
     * @return List<ParsedCPV> or null
     */
    private List<ParsedCPV> parseCpvs(final Element part2) {
        if (part2 == null) {
            return null;
        }

        final List<ParsedCPV> result = new ArrayList<>();

        // parse main cpvs
        final List<Element> mainCpvs = part2.select(
                "td:containsOwn(Fő tárgy:) ~ td");

        if (mainCpvs != null && !mainCpvs.isEmpty()) {
            for (Element cpv : mainCpvs) {
                if (!cpv.text().trim().isEmpty()) {
                    result.add(new ParsedCPV()
                            .setCode(cpv.text())
                            .setIsMain(String.valueOf(true)));
                }
            }
        }

        // parse other cpvs
        final List<Element> otherCpvs = part2.select(
                "table tr:has(td:containsOwn(Fő tárgy:)) ~ tr > td");

        if (otherCpvs != null && !otherCpvs.isEmpty()) {
            for (Element cpv : otherCpvs) {
                if (!cpv.text().trim().isEmpty() && !cpv.text().contains("További tárgyak")) {
                    result.add(new ParsedCPV()
                            .setCode(cpv.text())
                            .setIsMain(String.valueOf(false)));
                }
            }
        }

        return result.isEmpty() ? null : result;
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    /**
     * Get value of span in div or div in b element or div under dir, three types of forms.
     *
     * @param selector selector by which result is found
     * @param element  element to parse from
     *
     * @return value of span or div
     */
    private static String getValueMultipleWays(final String selector, final Element element) {
        String result = JsoupUtils.selectText("div:containsOwn(" + selector + ") span", element);

        if (result == null) {
            result = JsoupUtils.selectText("b:containsOwn(" + selector + ") span", element);
        }

        if (result == null) {
            result = JsoupUtils.selectText("* > div:has(b:containsOwn(" + selector + ")) + div span", element);
        }

        if (result == null) {
            result = JsoupUtils.selectText("* > div:has(div:containsOwn(" + selector + ")) + div span", element);
        }

        return result;
    }

    /**
     * Get value of span in div element.
     *
     * @param selectors selector by which div is found
     * @param element   element to parse from
     *
     * @return value of span
     */
    private static String getValueMultipleWays(final String[] selectors, final Element element) {
        for (String selector : selectors) {
            final String result = getValueMultipleWays(selector, element);

            if (result != null && !result.trim().isEmpty()) {
                return result;
            }
        }

        return null;
    }

    /**
     * Get value under one of these: value of span in div or div in b element or div under dir, three types of forms.
     *
     * @param selector selector by which result is found
     * @param element  element to parse from
     *
     * @return value of span or div
     */
    private static String getSecondValueMultipleWays(final String selector, final Element element) {
        String result = JsoupUtils.selectText("div:containsOwn(" + selector + ") span + *", element);

        if (result == null) {
            result = JsoupUtils.selectText("b:containsOwn(" + selector + ") span + *", element);
        }

        if (result == null) {
            result = JsoupUtils.selectText("* > div:has(b:containsOwn(" + selector + ")) + div span + *", element);
        }

        if (result == null) {
            result = JsoupUtils.selectText("* > div:has(div:containsOwn(" + selector + ")) + div span + *", element);
        }

        return result;
    }

    /**
     * Get value of div under div element.
     *
     * @param selector selector by which div is found
     * @param element  element to parse from
     *
     * @return value of span
     */
    private static String getDivUnderDiv(final String selector, final Element element) {
        return JsoupUtils.selectText("div:contains(" + selector + ") + div", element);
    }

    /**
     * Get value of div under div element.
     *
     * @param selectors selector by which div is found
     * @param element   element to parse from
     *
     * @return value of span
     */
    private static String getDivUnderDiv(final String[] selectors, final Element element) {
        for (String selector : selectors) {
            final String result = getDivUnderDiv(selector, element);

            if (result != null && !result.trim().isEmpty()) {
                return result;
            }
        }

        return null;
    }

    /**
     * Parse checked value.
     *
     * @param firstLine from which line to look for checked value
     * @param lastLine  to which line to look for checked value
     * @param element   element to search in
     *
     * @return String or null
     */
    private String getCheckedValue(final String firstLine, final String lastLine, final Element element) {
        Elements firstLines = element.select("div > div:has(div:containsOwn(" + firstLine + "))");
        final Element partBuyerType;

        if (firstLine.length() == 1) {
            partBuyerType = ParserUtils.getSubsectionOfElements(
                    firstLines.first(),
                    element.select("div > div:has(div:containsOwn(" + lastLine + "))").first());
        } else {
            partBuyerType = ParserUtils.getSubsectionOfElements(
                    element.select("div div > div > div:has(div:containsOwn(" + firstLine + "))").first(),
                    element.select("div div > div > div:has(div:containsOwn(" + lastLine + "))").first());
        }

        String result = JsoupUtils.selectText("span:has(b:containsOwn(x)) + span", partBuyerType);

        if (result == null || result.trim().isEmpty()) {
            result = JsoupUtils.selectOwnText("div div div:has(b:containsOwn(x))", partBuyerType);
        }

        return result;
    }

    /**
     * Get value of td under th element.
     *
     * @param selector selector by which div is found
     * @param element  element to parse from
     *
     * @return value of span
     */
    private static String getTdUnderTh(final String selector, final Element element) {
        final String result = JsoupUtils.selectText("th:containsOwn(" + selector + ") + td", element);
        return result == null || result.trim().isEmpty() ? null : result;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "HU";
    }
}
