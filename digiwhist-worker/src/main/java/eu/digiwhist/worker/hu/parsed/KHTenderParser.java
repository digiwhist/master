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
    private static final String SECOND_SECTION_START = "div > div:matches((?i)II(\\.|\\.A) SZAKASZ:)";
    private static final String THIRD_SECTION_START = "div > div:matches((?i)III(\\.|\\.A) SZAKASZ:)";
    private static final String FOURTH_SECTION_START = "div > div:matches((?i)IV(\\.|\\.A) SZAKASZ:)";
    private static final String LOT_SECTION_START = "div > div > div:contains(: Az eljárás eredménye)";
    private static final String ANY_SECTION_AFTER = " ~ div:matches(^(I|V)(\\.|I|V)(\\.\\ |\\ ).*)";
    private static final String ANY_SUBSECTION_AFTER = " ~ div:matches(^(I|V)(\\.|I|V).*)";

    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        final Document form = Jsoup.parse(rawTender.getSourceData());

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

        final Element part1 = ParserUtils.getSubsectionOfElements(
                document.select(FIRST_SECTION_START).first(),
                document.select(FIRST_SECTION_START + ANY_SECTION_AFTER).first());
        final Element part2 = ParserUtils.getSubsectionOfElements(
                document.select(SECOND_SECTION_START).first(),
                document.select(SECOND_SECTION_START + ANY_SECTION_AFTER).first());
        final Element part3 = ParserUtils.getSubsectionOfElements(
                document.select(THIRD_SECTION_START).first(),
                document.select(THIRD_SECTION_START + ANY_SECTION_AFTER).first());
        final Element part4 = ParserUtils.getSubsectionOfElements(
                document.select(FOURTH_SECTION_START).first(),
                document.select(FOURTH_SECTION_START + ANY_SECTION_AFTER).first());
        final Element partLots = ParserUtils.getSubsectionOfElements(
                document.select(LOT_SECTION_START).first(),
                document.select(LOT_SECTION_START + ANY_SECTION_AFTER).first());

        parsedTender
                .addBuyer(new ParsedBody()
                        .setName(getSpanInDiv("Hivatalos név", part1))
                        .addBodyId(new BodyIdentifier()
                                .setId(getSpanInDiv("Nemzeti azonosító", part1))
                                .setType(BodyIdentifier.Type.HEADER_ICO)
                                .setScope(BodyIdentifier.Scope.HU))
                        .setAddress(new ParsedAddress()
                                .setStreet(getSpanInDiv("Postai cím", part1))
                                .setCity(getSpanInDiv("Város", part1))
                                .setPostcode(getSpanInDiv("Postai irányítószám", part1))
                                .setCountry(getSpanInDiv("Ország", part1))
                                .setUrl(getSpanInDiv("Az ajánlatkérő általános címe (URL)", part1))
                                .addNuts(getSpanInDiv("NUTS", part1)))
                        .setContactName(getSpanInDiv(new String[]{
                                "Kapcsolattartó személy",
                                "Címzett"
                        }, part1))
                        .setPhone(getSpanInDiv("Telefon", part1))
                        .setEmail(getSpanInDiv("E-mail", part1))
                        .setBuyerType(getTdUnderTh("Ajánlatkérő típusa:", form))
                        .addMainActivity(getTdUnderTh("Ajánlatkérő fő tevényeségi köre:", part1)))
                .setIsOnBehalfOf(getSpanInDiv("Az ajánlatkérő más ajánlatkérők nevében végzi a beszerzést", part1))
                .setTitle(getSpanInDiv("Elnevezés", part2))
                .setDocumentsDeadline(JsoupUtils.selectText("div:contains(A dokumentáció beszerzésének határideje) +" +
                        " div div:containsOwn(Dátum:) > span", part4))
                .setAddressOfImplementation(new ParsedAddress()
                        .setRawAddress(getDivUnderDiv("A teljesítés helye:", part2))
                        .addNuts(getSpanInDiv("NUTS-kód", part2)))
                .setDescription(getSpanInDiv("A szerződés vagy a beszerzés(ek) rövid meghatározá", part2))
                .setCpvs(parseCpvs(part2))
                .setFinalPrice(parseFinalPrice(part2))
                .setAwardCriteria(parseAwardCriteria(part3))
                .addPublication(parsePreviousPublication(part3))
                .addPublication(parseTedPublication(part3))
                .setLots(parseLots(partLots));

        return new ArrayList<>(Collections.singletonList(parsedTender));
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
        final Elements criteria = JsoupUtils.select("div:contains(Az összességében legelőnyösebb ajánlat a következő" +
                " részszempontok alapján) + div tbody > tr", element);

        // First line is header, thus size has to be at least 2
        if (criteria == null || criteria.size() < 2) {
            return null;
        }

        final List<ParsedAwardCriterion> result = new ArrayList<>();
        for (int iterator = 1; criteria.size() > iterator; iterator++) {
            result.add(new ParsedAwardCriterion()
                    .setName(JsoupUtils.selectText("td", criteria.get(iterator)))
                    .setWeight(JsoupUtils.selectText("td + td", criteria.get(iterator)))
            );
        }

        return result;
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
            result.add(new ParsedTenderLot()
                    .setPositionOnPage(String.valueOf(positionOnPage++))
                    .setLotNumber(getSpanInDiv("A szerződés száma:", lot))
                    .setTitle(JsoupUtils.selectText("div > div:contains(A szerződés száma:) span + span" +
                            " + span", lot))
                    .setAwardDecisionDate(getSpanInDiv("A szerződés megkötésének dátuma:", lot))
                    .setBidsCount(getSpanInDiv("A beérkezett ajánlatok szám", lot))
                    .setElectronicBidsCount(getSpanInDiv("Elektronikus úton beérkezett ajánlatok száma", lot))
                    .addBid(new ParsedBid()
                            .addBidder(new ParsedBody()
                                    .setName(getSpanInDiv("Hivatalos név:", lot))
                                    .setAddress(new ParsedAddress()
                                            .setStreet(getSpanInDiv("Postai cím:", lot))
                                            .setCity(getSpanInDiv("Város:", lot))
                                            .setPostcode(getSpanInDiv("ostai irányítószám:", lot))
                                            .setCountry(getSpanInDiv("Ország:", lot))
                                            .addNuts(getSpanInDiv("NUTS-kód", lot))
                                            .setUrl(getSpanInDiv("Internetcím(ek)", lot)))
                                    .setEmail(getSpanInDiv("E-mail:", lot))
                                    .setPhone(getSpanInDiv("Telefon:", lot)))));
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
        final String price = getSpanInDiv("Érték:", part2);

        if (price == null || price.trim().isEmpty()) {
            return null;
        }

        final String currency = getSpanInDiv("Pénznem:", part2);
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
        final List<ParsedCPV> result = new ArrayList<>();

        // parse main cpvs
        final List<Element> mainCpvs = part2.select(
                "div:containsOwn(Fő CPV-kód) td:containsOwn(Fő tárgy:) ~ td");

        if (mainCpvs != null && !mainCpvs.isEmpty()) {
            for (Element cpv : mainCpvs) {
                if (!cpv.toString().trim().isEmpty()) {
                    result.add(new ParsedCPV()
                            .setCode(cpv.toString())
                            .setIsMain(String.valueOf(true)));
                }
            }
        }

        // parse other cpvs
        final List<Element> otherCpvs = part2.select(
                "div:containsOwn(További CPV-kód) td:containsOwn(Fő tárgy:) ~ td");

        if (otherCpvs != null && !otherCpvs.isEmpty()) {
            for (Element cpv : otherCpvs) {
                if (!cpv.toString().trim().isEmpty()) {
                    result.add(new ParsedCPV()
                            .setCode(cpv.toString())
                            .setIsMain(String.valueOf(true)));
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
     * Get value of span in div element.
     *
     * @param selector selector by which div is found
     * @param element  element to parse from
     *
     * @return value of span
     */
    private static String getSpanInDiv(final String selector, final Element element) {
        return JsoupUtils.selectText("div:containsOwn(" + selector + ") span", element);
    }

    /**
     * Get value of span in div element.
     *
     * @param selectors selector by which div is found
     * @param element   element to parse from
     *
     * @return value of span
     */
    private static String getSpanInDiv(final String[] selectors, final Element element) {
        for (String selector : selectors) {
            final String result = getSpanInDiv(selector, element);

            if (result != null && !result.trim().isEmpty()) {
                return result;
            }
        }

        return null;
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
     * Get value of td under th element.
     *
     * @param selector selector by which div is found
     * @param element  element to parse from
     *
     * @return value of span
     */
    private static String getTdUnderTh(final String selector, final Element element) {
        return JsoupUtils.selectText("th:containsOwn(" + selector + ") + td", element);
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "HU";
    }
}
