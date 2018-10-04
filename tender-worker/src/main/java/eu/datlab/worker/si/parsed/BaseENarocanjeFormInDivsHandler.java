package eu.datlab.worker.si.parsed;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.StringUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static eu.datlab.worker.si.parsed.ENarocanjeTenderParser.NBSP_CHARACTER;

import java.util.ArrayList;
import java.util.List;
import org.jsoup.nodes.Node;

import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * Base handler for parsing forms in which information are saved in div HTML elements.
 */
abstract class BaseENarocanjeFormInDivsHandler {
    private static final Logger logger = LoggerFactory.getLogger(BaseENarocanjeFormInDivsHandler.class);

    static final String SUBSECTION_I_1_CONTENT_SELECTOR = "div.tab-content > div > h5:containsOwn(I.1"
            + NBSP_CHARACTER + "Ime in naslovi) + div";
    static final String SUBSECTION_I_4_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(I.4"
            + NBSP_CHARACTER + "Vrsta javnega naročnika)";
    // probably form contains subsection I.5 or I.6, not both. And in both cases there is the same info
    static final String SUBSECTION_I_5_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(I.5"
            + NBSP_CHARACTER + "Glavna področja dejavnosti)";
    static final String SUBSECTION_I_6_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(I.6"
            + NBSP_CHARACTER + "Glavna področja dejavnosti)";
    static final String SUBSECTION_II_1_1_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.1.1"
            + NBSP_CHARACTER + "Naslov)";
    static final String SUBSECTION_II_1_2_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.1.2"
            + NBSP_CHARACTER + "Glavna koda CPV)";
    static final String SUBSECTION_II_1_2_CONTENT_SELECTOR = SUBSECTION_II_1_2_TITLE_SELECTOR + " + table";
    static final String SUBSECTION_II_1_3_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.1.3"
            + NBSP_CHARACTER + "Vrsta naročila)";
    static final String SUBSECTION_II_1_4_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.1.4"
            + NBSP_CHARACTER + "Kratek opis)";
    static final String SUBSECTION_II_1_5_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.1.5"
            + NBSP_CHARACTER + "Ocenjena skupna vrednost)";
    static final String SUBSECTION_II_1_6_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.1.6"
            + NBSP_CHARACTER + "Informacije o sklopih)";
    static final String SUBSECTION_II_1_7_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.1.7"
            + NBSP_CHARACTER + "Skupna vrednost javnega naročila)";
    static final String SUBSECTION_II_2_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.2"
            + NBSP_CHARACTER + "Opis)";
    static final String SUBSECTION_III_1_1_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(III.1.1"
            + NBSP_CHARACTER + "Ustreznost za opravljanje poklicne dejavnosti, vključno z " +
            "zahtevami v zvezi z vpisom v register poklicev ali trgovski register)";
    static final String SUBSECTION_III_1_2_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(III.1.2"
            + NBSP_CHARACTER + "Poslovno in finančno stanje";
    static final String SUBSECTION_III_1_3_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(III.1.3"
            + NBSP_CHARACTER + "Tehnična in strokovna sposobnost";
    static final String SUBSECTION_III_1_4_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(III.1.4"
            + NBSP_CHARACTER + "Objektivna pravila in merila za sodelovanje";
    static final String SUBSECTION_IV_1_1_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.1.1"
            + NBSP_CHARACTER + "Vrsta postopka)";
    static final String SUBSECTION_IV_1_3_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.1.3"
            + NBSP_CHARACTER
            + "Informacije o okvirnem sporazumu ali dinamičnem nabavnem sistemu)";
    static final String SUBSECTION_IV_1_4_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.1.4"
            + NBSP_CHARACTER
            + "Informacije o zmanjšanju števila rešitev ali ponudb med pogajanji ali dialogom)";
    static final String SUBSECTION_IV_1_8_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.1.8"
            + NBSP_CHARACTER + "Informacije o Sporazumu o vladnih naročilih)";
    static final String SUBSECTION_IV_2_1_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.2.1"
            + NBSP_CHARACTER + "Prejšnja objava v zvezi s tem postopkom)";
    static final String SUBSECTION_IV_2_2_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.2.2"
            + NBSP_CHARACTER + "Rok za prejem ponudb ali prijav za sodelovanje)";
    static final String SUBSECTION_IV_2_3_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.2.3"
            + NBSP_CHARACTER
            + "Predvideni datum pošiljanja povabil k oddaji ponudbe ali sodelovanju izbranim kandidatom)";
    static final String SUBSECTION_IV_2_4_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.2.4"
            + NBSP_CHARACTER
            + "Jeziki, v katerih se predložijo ponudbe ali prijave za sodelovanje)";
    static final String SUBSECTION_IV_2_6_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.2.6"
            + NBSP_CHARACTER
            + "Minimalni časovni okvir, v katerem mora ponudnik zagotavljati veljavnost ponudbe)";
    static final String SUBSECTION_IV_2_7_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.2.7"
            + NBSP_CHARACTER + "Način odpiranja ponudb)";
    static final String SUBSECTION_VI_5_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(VI.5"
            + NBSP_CHARACTER + "Datum pošiljanja tega obvestila)";

    /**
     * Parses common attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from form
     */
    static ParsedTender parseCommonFormInDivsAttributes(final ParsedTender tender, final Element form) {
        final Element sectionII17TitleElement = JsoupUtils.selectFirst(SUBSECTION_II_1_7_TITLE_SELECTOR, form);
        final Element sectionII2TitleElement = JsoupUtils.selectFirst(SUBSECTION_II_2_TITLE_SELECTOR, form);
        final Element sectionII15TitleElement = JsoupUtils.selectFirst(SUBSECTION_II_1_5_TITLE_SELECTOR, form);
        final Element sectionII16TitleElement = JsoupUtils.selectFirst(SUBSECTION_II_1_6_TITLE_SELECTOR, form);
        final Element sectionI1 = JsoupUtils.selectFirst(SUBSECTION_I_1_CONTENT_SELECTOR, form);
        final Element sectionI4 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_I_4_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_I_5_TITLE_SELECTOR, form));        
        final Element sectionII11 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_II_1_1_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_II_1_2_TITLE_SELECTOR, form));
        final Element sectionII12 = JsoupUtils.selectFirst(SUBSECTION_II_1_2_CONTENT_SELECTOR, form);
        final Element sectionII14 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_II_1_4_TITLE_SELECTOR, form),
                sectionII15TitleElement == null ? sectionII16TitleElement : sectionII15TitleElement);
        final Element sectionII16 = ParserUtils.getSubsectionOfNodes(
                sectionII16TitleElement,
                sectionII17TitleElement == null ? sectionII2TitleElement : sectionII17TitleElement);
        final Element sectionIII11 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_III_1_1_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_III_1_2_TITLE_SELECTOR, form));
        final Element sectionIII12 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_III_1_2_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_III_1_3_TITLE_SELECTOR, form));
        final Element sectionIII13 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_III_1_3_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_III_1_4_TITLE_SELECTOR, form));
        final Element sectionIV11 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_IV_1_1_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_IV_1_3_TITLE_SELECTOR, form));
        final Element sectionIV13 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_IV_1_3_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_IV_1_4_TITLE_SELECTOR, form));
        final Element sectionIV18 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_IV_1_8_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_IV_2_1_TITLE_SELECTOR, form));
        final Element sectionIV21 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_IV_2_1_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_IV_2_2_TITLE_SELECTOR, form));
        final Element sectionIV24 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_IV_2_4_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_IV_2_6_TITLE_SELECTOR, form));
        final Element sectionVI5 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_VI_5_TITLE_SELECTOR, form),
                null);

        String procedureType = sectionIV11 == null ? null : StringUtils.removeDotsAtTheEnd(sectionIV11.ownText());

        Element buyerActivityNode = ParserUtils.getSubsectionOfNodes(
            JsoupUtils.selectFirst(SUBSECTION_I_5_TITLE_SELECTOR, form),
            JsoupUtils.selectFirst(SUBSECTION_I_6_TITLE_SELECTOR, form));
        if (buyerActivityNode == null) {
            buyerActivityNode = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_I_6_TITLE_SELECTOR, form), null);
        }

        tender
                .addBuyer(parseBuyer(sectionI1, sectionI4, buyerActivityNode))
                .setTitle(ParserUtils.getFromContent(sectionII11, null, "Naslov:"))
                .setCpvs(parseTenderCpvs(sectionII12))
                .setNationalProcedureType(procedureType)
                .setProcedureType(procedureType)
                .setLots(parseLots(form))
                .setHasLots(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                        ParserUtils.getFromContent(sectionII16, null, "Naročilo je razdeljeno na sklope:"))))
                .setIsFrameworkAgreement(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                        ParserUtils.getFromContent(sectionIV13, null, "sporazum:"))))
                .setDescription(sectionII14.ownText())
                .setIsCoveredByGpa(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                        ParserUtils.getFromContent(sectionIV18, null,
                                "Naročilo ureja Sporazum o vladnih naročilih:"))))
                .setPersonalRequirements(sectionIII11 == null ? null : sectionIII11.ownText())
                .setEconomicRequirements(sectionIII12 == null ? null : sectionIII12.ownText())
                .setTechnicalRequirements(sectionIII13 == null ? null : sectionIII13.ownText())
                .addEligibleBidLanguage(sectionIV24 == null ? null : sectionIV24.ownText());

        assert tender.getPublications().get(0).getIsIncluded().equals(Boolean.toString(true));
        tender.getPublications().get(0)
                .setBuyerAssignedId(ParserUtils.getFromContent(sectionII11, null, "Referenčna številka dokumenta:"))
                .setPublicationDate(sectionVI5.ownText());

        parseDispatchDateOfPreviousPublication(tender, sectionIV21);

        return tender;
    }

    /**
     * Parse estimated price value from document.
     *
     * @param section
     *         html for section where the price is
     *
     * @return estimated price or Null
     */
    static ParsedPrice parseEstimatedPrice(final Element section) {
        if (section == null) {
            // e.g. in II.1.5 in https://www.enarocanje.si/Obrazci/?id_obrazec=184804
            return null;
        }

        String priceString = section.ownText();

        if (priceString.isEmpty()) {
            // e.g. in II.2.6 in https://www.enarocanje.si/Obrazci/?id_obrazec=180769
            return null;
        }

        // example of price: "Vrednost brez DDV: 1.006.200,00 EUR"

        final String includingVatString = "Vrednost brez DDV:";
        final boolean includingVat = priceString.startsWith(includingVatString);

        // we have found only prices with included VAT yet
        if (!includingVat) {
            throw new UnrecoverableException("We found string for excluding VAT of estimated price. It is in "
                    + priceString);
        }

        // get just price and currency
        final String[] vatInfoAndPriceWithCurrency = priceString.split(":");
        assert vatInfoAndPriceWithCurrency.length == 2;
        priceString = vatInfoAndPriceWithCurrency[1].trim();

        final String[] priceAndCurrency = priceString.split(" ");
        assert priceAndCurrency.length == 2;

        final ParsedPrice estimatedPrice = includingVat
                ? new ParsedPrice().setAmountWithVat(priceAndCurrency[0])
                : new ParsedPrice().setNetAmount(priceAndCurrency[0]);
        return estimatedPrice.setCurrency(priceAndCurrency[1]);
    }

    /**
     * Parses common body attributes.
     *
     * @param bodyElement
     *         html for body info
     *
     * @return parsed common body info
     */
    static ParsedBody parseCommonBodyAttributes(final Element bodyElement) {
        return new ParsedBody()
                .setName(JsoupUtils.selectText("label:containsOwn(Uradno ime) + div", bodyElement))
                .setAddress(new ParsedAddress()
                        .setStreet(JsoupUtils.selectOwnText(
                                "div > div > div > div:has(label:containsOwn(Poštni naslov:))", bodyElement))
                        .setPostcode(JsoupUtils.selectOwnText(
                                "div > div > div > div:has(label:containsOwn(Poštna številka:))", bodyElement))
                        .setCity(JsoupUtils.selectOwnText("div > div > div > div:has(label:containsOwn(Kraj))",
                                bodyElement))
                        .setCountry(JsoupUtils.selectOwnText("div > div > div > div:has(label:containsOwn(Država))",
                                bodyElement))
                        .addNuts(JsoupUtils.selectOwnText(
                                "div > div > div > div > div:has(label:containsOwn(Šifra NUTS:))", bodyElement)))
                .setContactPoint(JsoupUtils.selectOwnText(
                        "div > div > div > div:has(label:containsOwn(Kontaktna oseba))", bodyElement))
                .setEmail(JsoupUtils.selectOwnText("div > div > div > div:has(label:containsOwn(E-pošta))",
                        bodyElement))
                .setPhone(JsoupUtils.selectOwnText("div > div > div > div:has(label:containsOwn(Telefon))",
                        bodyElement));
    }

    /**
     * Parse tender CPVs from document.
     *
     * @param sectionII12
     *         html for section II.1.2
     *
     * @return List of parsed CPVs
     */
    private static List<ParsedCPV> parseTenderCpvs(final Element sectionII12) {
        return parseCpvs(sectionII12);
    }

    /**
     * Parse CPVs from table.
     *
     * @param tableElement
     *         table element where CPVs are
     *
     * @return List of parsed CPVs
     */
    private static List<ParsedCPV> parseCpvs(final Element tableElement) {
        final List<ParsedCPV> cpvs = new ArrayList<>();

        // the table has main and supplementary CPV code on each row. See
        // https://www.enarocanje.si/Obrazci/?id_obrazec=190535

        // main CPV
        Elements mainCpvCodeElements = JsoupUtils.select("tbody > tr > td:nth-child(1)", tableElement);
        mainCpvCodeElements
                .stream()
                .filter(e -> !e.ownText().isEmpty())
                .forEach(cpvElement -> {
                    cpvs.add(new ParsedCPV()
                            .setIsMain(Boolean.TRUE.toString())
                            .setCode(cpvElement.ownText()));
                });

        // other CPV
        Elements supplementaryCpvCodeElements = JsoupUtils.select("tbody > tr > td:nth-child(2)", tableElement);
        supplementaryCpvCodeElements
                .stream()
                .filter(e -> !e.ownText().isEmpty())
                .forEach(cpvElement -> {
                    cpvs.add(new ParsedCPV()
                            .setIsMain(Boolean.FALSE.toString())
                            .setCode(cpvElement.ownText()));
                });

        return cpvs;
    }

    /**
     * Parses all the lots from document.
     *
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return list of all parsed lots or empty list if no lots specified
     */
    private static List<ParsedTenderLot> parseLots(final Element form) {
        final Elements lotElements = JsoupUtils.select("div.tab-content > div > div > div > h5:containsOwn(II.2"
                + NBSP_CHARACTER + "Opis) ~ div", form);
        if (lotElements.isEmpty()) {
            return null;
        }

        final String lotTitleSelector = "h4:containsOwn(Sklop)";
        final String sectionII21TitleSelector = "h5:containsOwn(II.2.1" + NBSP_CHARACTER + "Naslov)";
        final String sectionII22TitleSelector = "h5:containsOwn(II.2.2" + NBSP_CHARACTER + "Dodatna(-e) koda(-e) CPV)";
        final String sectionII22TableSelector = sectionII22TitleSelector + " + table";
        final String sectionII23TitleSelector = "h5:containsOwn(II.2.3" + NBSP_CHARACTER + "Kraj izvedbe)";
        final String sectionII24TitleSelector = "h5:containsOwn(II.2.4" + NBSP_CHARACTER + "Opis javnega naročila)";
        final String sectionII25TitleSelector = "h5:containsOwn(II.2.5" + NBSP_CHARACTER + "Merila za izbiro ponudbe)";
        final String sectionII26TitleSelector = "h5:containsOwn(II.2.6" + NBSP_CHARACTER + "Ocenjena vrednost)";
        final String sectionII27TitleSelector = "h5:containsOwn(II.2.7" + NBSP_CHARACTER + "Trajanje naročila)";
        final String sectionII29TitleSelector = "h5:containsOwn(II.2.9" + NBSP_CHARACTER
                + "Informacije o omejitvah števila kandidatov, ki bodo povabljeni k sodelovanju)";
        final String sectionII210TitleSelector = "h5:containsOwn(II.2.10" + NBSP_CHARACTER + "Informacije o variantah)";
        final String sectionII211TitleSelector = "h5:containsOwn(II.2.11" + NBSP_CHARACTER + "Informacije o variantah)";
        final String sectionII213TitleSelector = "h5:containsOwn(II.2.13" + NBSP_CHARACTER + "Informacije o sredstvih"
            + " EU)";
        final String sectionII214TitleSelector = "h5:containsOwn(II.2.14" + NBSP_CHARACTER + "Dodatne informacije)";
        
        List<ParsedTenderLot> lots = new ArrayList<>();
        for (Element lotElement : lotElements) {
            final Element sectionII21 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionII21TitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionII22TitleSelector, lotElement));
            final Element sectionII23 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionII23TitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionII24TitleSelector, lotElement));
            final Element sectionII24 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionII24TitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionII25TitleSelector, lotElement));
            final Element sectionII25 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionII25TitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionII26TitleSelector, lotElement));
            final Element sectionII26 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionII26TitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionII27TitleSelector, lotElement));
            // last node of section II.2.7 is title of II.2.9, because I have not found section II.2.8
            final Element sectionII27 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionII27TitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionII29TitleSelector, lotElement));
            final Element sectionII210 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionII210TitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionII211TitleSelector, lotElement));
            final Element sectionII211 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionII211TitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionII213TitleSelector, lotElement));
            final Element sectionII213 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionII213TitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionII214TitleSelector, lotElement));

            // get lot number
            final String lotNumber;
            final String lotNumberInSectionII21 = ParserUtils.getFromContent(sectionII21, null, "Številka sklopa:");
            if (lotNumberInSectionII21 != null) {
                if (lots.stream().anyMatch(l -> l.getLotNumber().equals(lotNumberInSectionII21))) {
                    final String lotNumberInTitle = ParserUtils.getFromContent(
                            JsoupUtils.selectFirst(lotTitleSelector, lotElement), null, "Sklop");
                    assert lotNumberInTitle != null;
                    if (lots.stream().anyMatch(l -> l.getLotNumber().equals(lotNumberInTitle))) {
                        logger.warn("Lot numbers in first section ({}) and in title ({}) are used by different lot(s). "
                                + "The lot is skipped", lotNumberInSectionII21, lotNumberInTitle);
                        continue;
                    } else {
                        logger.warn("Lot number {} in first section is used by different lot. We use lot number {} " +
                                        "in title", lotNumberInSectionII21, lotNumberInTitle);
                        lotNumber = lotNumberInTitle;
                    }
                } else {
                    lotNumber = lotNumberInSectionII21;
                }
            } else {
                // lot number is not filled when there is only one lot. See
                // https://www.enarocanje.si/Obrazci/?id_obrazec=179061
                assert lotElements.size() == 1;
                lotNumber = null;
            }

            ParsedTenderLot lot = new ParsedTenderLot()
                .setTitle(ParserUtils.getFromContent(sectionII21, null, "Naslov:"))
                .setLotNumber(lotNumber)
                .setCpvs(parseCpvs(JsoupUtils.selectFirst(sectionII22TableSelector, lotElement)))
                .setEstimatedPrice(parseEstimatedPrice(sectionII26))
                .addFunding(new ParsedFunding()
                    .setIsEuFund(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                        ParserUtils.getFromContent(sectionII213, null, ENarocanjeTenderFormUtils.IS_EU_FUND_TITLE)))))
                .setEstimatedDurationInDays(ParserUtils.getFromContent(sectionII27, null,
                    ENarocanjeTenderFormUtils.DURATION_IN_DAYS_TITLE))
                .setEstimatedDurationInMonths(ParserUtils.getFromContent(sectionII27, null, "Trajanje v mesecih:"))
                .setAreVariantsAccepted(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                    ParserUtils.getFromContent(sectionII210, null, "Variante so dopustne:"))))
                .setHasOptions(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                    ParserUtils.getFromContent(sectionII211, null, "Variante:"))))
                // section II.2.4 is not always presented. See
                // https://www.enarocanje.si/Obrazci/?id_obrazec=216535
                .setDescription(sectionII24 == null ? null : sectionII24.ownText())
                .setSelectionMethod(sectionII25 == null ? null : StringUtils.removeDotsAtTheEnd(sectionII25.ownText()))
                .setAwardCriteria(parseLotAwardCriteria(sectionII25))
                .setEstimatedStartDate(ParserUtils.getFromContent(sectionII27, null, "Začetek:"))
                .setEstimatedCompletionDate(ParserUtils.getFromContent(sectionII27, null, "Konec:"));

            // raw address
            if (sectionII23 != null) {
                String[] addr = sectionII23.ownText().split("Glavna lokacija ali kraj izvedbe:");
                String rawAddr; List<String> nuts = null;
                if (addr.length > 1) {
                    nuts = Arrays.asList(addr[0].split(",")).stream()
                        .map(n -> n.split("\\-")[0].trim())
                        .filter(n -> !n.isEmpty())
                        .collect(Collectors.toList());
                    rawAddr = addr[1];
                } else {
                    rawAddr = addr[0];
                }                
                lot.setAddressOfImplementation(new ParsedAddress().setRawAddress(rawAddr).setNuts(nuts));
            }

            Element additionLotInfo = JsoupUtils.selectFirst("h4:containsOwn(Oddelek V: Oddaja naročila)"
                + " ~ div:has(h4:containsOwn(Sklop " + lotNumber + "))", form);
            if (additionLotInfo != null) {
                Element n = JsoupUtils.selectFirst("h4:containsOwn(Sklop)", additionLotInfo);
                Node sibl = n.nextSibling();
                while (sibl != null) {
                    if (sibl.toString().contains("Številka naročila:")) {
                        lot.setContractNumber(sibl.toString().split(":")[1].trim());
                    }
                    if (sibl.toString().contains("V.2.1&nbsp;Datum sklenitve pogodbe")) {
                        sibl = sibl.nextSibling();
                        if (sibl.nodeName().equals("#text")) {
                            lot.setAwardDecisionDate(sibl.toString());
                        }
                    }
                    if (sibl.toString().contains("Število elektronsko prejetih ponudb:")) {
                        lot.setElectronicBidsCount(sibl.toString().split(":")[1].trim());
                    }

                    sibl = sibl.nextSibling();
                }
            }

            lots.add(lot);
        }

        return lots;
    }

    /**
     * @param node
     *      node to parse from
     * @return non-empty list of award criteria or null
     */
    static List<ParsedAwardCriterion> parseLotAwardCriteria(final Element node) {
        List<ParsedAwardCriterion> criteria = new ArrayList<>();

        Elements rows = JsoupUtils.select("table tr:gt(0)", node);
        if (rows != null) {
            rows.forEach(n -> criteria.add(new ParsedAwardCriterion()
                .setName(n.child(0).text())
                .setWeight(n.child(1).text())));
        }

        Element price = JsoupUtils.selectFirst("table", node);
        if (price != null) {
            Node sibl = price.nextSibling();
            while (sibl != null && !sibl.nodeName().equals("h5")) {
                // text node with number is price criterion weight
                if (sibl.nodeName().equals("#text") && sibl.toString().trim().matches("[0-9]+")) {
                    criteria.add(new ParsedAwardCriterion()
                        .setName("Cena")
                        .setWeight(sibl.toString())
                        .setIsPriceRelated(Boolean.TRUE.toString()));
                }

                sibl = sibl.nextSibling();
            }
        }

        return criteria.isEmpty() ? null : criteria;
    }

    /**
     * Parses buyer info.
     *
     * @param sectionI1
     *         html for section I.1 (with buyer info)
     * @param sectionI4
     *         html for section I.4 (with buyer info)
     * @param sectionI5
     *         html for section I.5 (with buyer info)
     * @return parsed buyer info
     */
    private static ParsedBody parseBuyer(final Element sectionI1, final Element sectionI4, final Element sectionI5) {
        final ParsedBody buyer = parseCommonBodyAttributes(sectionI1);

        buyer.getAddress()
                .setUrl(JsoupUtils.selectText("label:containsOwn(Glavni naslov (URL):) + a", sectionI1));
        
        return buyer
                .setBuyerType(sectionI4 == null ? null : StringUtils.removeDotsAtTheEnd(sectionI4.ownText()))
                .addMainActivity(sectionI5 == null ? null : StringUtils.removeDotsAtTheEnd(sectionI5.ownText()));
    }

    /**
     * Parses dispatch date of previous publication and sets it to corresponding publication according to its source ID.
     *
     * @param tender
     *         parsed tender to be set
     * @param sectionIV21
     *         html for section IV.2.1
     */
    private static void parseDispatchDateOfPreviousPublication(final ParsedTender tender, final Element sectionIV21) {
        if (sectionIV21 == null) {
            return;
        }

        final String previousPublicationSourceId = ParserUtils.getFromContent(sectionIV21, null, "Številka objave:");

        if (previousPublicationSourceId == null) {
            // https://www.enarocanje.si/Obrazci/?id_obrazec=211835
            return;
        }

        final ParsedPublication previousPublication = tender.getPublications()
                .stream()
                .filter(p -> p.getSourceId().equals(previousPublicationSourceId))
                .findFirst()
                .orElse(null);
        previousPublication
                .setDispatchDate(ParserUtils.getFromContent(sectionIV21, null,
                        "Datum odpošiljanja izvirnega obvestila:"));
    }

}
