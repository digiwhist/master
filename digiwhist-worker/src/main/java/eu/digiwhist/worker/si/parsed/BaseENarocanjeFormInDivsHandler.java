package eu.digiwhist.worker.si.parsed;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Base handler for parsing forms in which information are saved in div HTML elements.
 */
abstract class BaseENarocanjeFormInDivsHandler {
    private static final Logger logger = LoggerFactory.getLogger(BaseENarocanjeFormInDivsHandler.class);

    static final String SUBSECTION_I_1_CONTENT_SELECTOR = "div.tab-content > div > h5:containsOwn(I.1"
            + ENarocanjeTenderParser.NBSP_CHARACTER + "Ime in naslovi) + div";
    static final String SUBSECTION_I_4_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(I.4"
            + ENarocanjeTenderParser.NBSP_CHARACTER + "Vrsta javnega naročnika)";
    static final String SUBSECTION_I_5_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(I.5"
            + ENarocanjeTenderParser.NBSP_CHARACTER + "Glavna področja dejavnosti)";
    static final String SUBSECTION_II_1_1_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.1.1"
            + ENarocanjeTenderParser.NBSP_CHARACTER + "Naslov)";
    static final String SUBSECTION_II_1_2_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.1.2"
            + ENarocanjeTenderParser.NBSP_CHARACTER + "Glavna koda CPV)";
    static final String SUBSECTION_II_1_2_CONTENT_SELECTOR = SUBSECTION_II_1_2_TITLE_SELECTOR + " + table";
    static final String SUBSECTION_II_1_5_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.1.5"
            + ENarocanjeTenderParser.NBSP_CHARACTER + "Ocenjena skupna vrednost)";
    static final String SUBSECTION_II_1_6_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.1.6"
            + ENarocanjeTenderParser.NBSP_CHARACTER + "Informacije o sklopih)";
    static final String SUBSECTION_II_1_7_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.1.7"
            + ENarocanjeTenderParser.NBSP_CHARACTER + "Skupna vrednost javnega naročila)";
    static final String SUBSECTION_II_2_TITLE_SELECTOR = "div.tab-content > div > div > div > h5:containsOwn(II.2"
            + ENarocanjeTenderParser.NBSP_CHARACTER + "Opis)";
    static final String SUBSECTION_IV_1_1_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.1.1"
            + ENarocanjeTenderParser.NBSP_CHARACTER + "Vrsta postopka)";
    static final String SUBSECTION_IV_1_3_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.1.3"
            + ENarocanjeTenderParser.NBSP_CHARACTER
            + "Informacije o okvirnem sporazumu ali dinamičnem nabavnem sistemu)";
    static final String SUBSECTION_IV_1_4_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.1.4"
            + ENarocanjeTenderParser.NBSP_CHARACTER
            + "Informacije o zmanjšanju števila rešitev ali ponudb med pogajanji ali dialogom)";
    static final String SUBSECTION_IV_2_2_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.2.2"
            + ENarocanjeTenderParser.NBSP_CHARACTER + "Rok za prejem ponudb ali prijav za sodelovanje)";
    static final String SUBSECTION_IV_2_3_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.2.3"
            + ENarocanjeTenderParser.NBSP_CHARACTER
            + "Predvideni datum pošiljanja povabil k oddaji ponudbe ali sodelovanju izbranim kandidatom)";
    static final String SUBSECTION_IV_2_6_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.2.6"
            + ENarocanjeTenderParser.NBSP_CHARACTER
            + "Minimalni časovni okvir, v katerem mora ponudnik zagotavljati veljavnost ponudbe)";
    static final String SUBSECTION_IV_2_7_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(IV.2.7"
            + ENarocanjeTenderParser.NBSP_CHARACTER + "Način odpiranja ponudb)";
    static final String SUBSECTION_VI_5_TITLE_SELECTOR = "div.tab-content > div > h5:containsOwn(VI.5"
            + ENarocanjeTenderParser.NBSP_CHARACTER + "Datum pošiljanja tega obvestila)";

    /**
     * Parses common attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from Form
     */
    static ParsedTender parseCommonAttributes(final ParsedTender tender, final Element form) {
        final Element sectionII17TitleElement = JsoupUtils.selectFirst(SUBSECTION_II_1_7_TITLE_SELECTOR, form);
        final Element sectionII2TitleElement = JsoupUtils.selectFirst(SUBSECTION_II_2_TITLE_SELECTOR, form);
        final Element sectionI1 = JsoupUtils.selectFirst(SUBSECTION_I_1_CONTENT_SELECTOR, form);
        final Element sectionI4 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_I_4_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_I_5_TITLE_SELECTOR, form));
        final Element sectionII11 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_II_1_1_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_II_1_2_TITLE_SELECTOR, form));
        final Element sectionII12 = JsoupUtils.selectFirst(SUBSECTION_II_1_2_CONTENT_SELECTOR, form);
        final Element sectionII16 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_II_1_6_TITLE_SELECTOR, form),
                sectionII17TitleElement == null ? sectionII2TitleElement : sectionII17TitleElement);
        final Element sectionIV11 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_IV_1_1_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_IV_1_3_TITLE_SELECTOR, form));
        final Element sectionIV13 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_IV_1_3_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_IV_1_4_TITLE_SELECTOR, form));
        final Element sectionVI5 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_VI_5_TITLE_SELECTOR, form),
                null);

        tender
                .addBuyer(parseBuyer(sectionI1, sectionI4))
                .setTitle(ParserUtils.getFromContent(sectionII11, null, "Naslov:"))
                .setCpvs(parseTenderCpvs(sectionII12))
                .setNationalProcedureType(sectionIV11 == null ? null : sectionIV11.ownText())
                .setLots(parseLots(form))
                .setHasLots(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                        ParserUtils.getFromContent(sectionII16, null, "Naročilo je razdeljeno na sklope:"))))
                .setIsFrameworkAgreement(
                        JsoupUtils.exists("h5:containsOwn(Informacije o okvirnem sporazumu)", sectionIV13).toString());

        assert tender.getPublications().get(0).getIsIncluded().equals(Boolean.toString(true));
        tender.getPublications().get(0)
                .setBuyerAssignedId(ParserUtils.getFromContent(sectionII11, null,
                        "Referenčna številka dokumenta:"))
                .setDispatchDate(sectionVI5.ownText());

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
                                bodyElement)))
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

        // the table has just one row with all CPV codes

        // main CPV
        cpvs.add(new ParsedCPV()
                .setIsMain(Boolean.TRUE.toString())
                .setCode(JsoupUtils.selectText("tbody > tr > td:nth-child(1)", tableElement)));

        // other CPV
        String additionalObjectsString = JsoupUtils.selectText("tbody > tr > td:nth-child(2)", tableElement);
        if (!additionalObjectsString.isEmpty()) {
            cpvs.add(new ParsedCPV()
                    .setIsMain(Boolean.FALSE.toString())
                    .setCode(additionalObjectsString));
        }

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
                + ENarocanjeTenderParser.NBSP_CHARACTER + "Opis) ~ div", form);

        if (lotElements.isEmpty()) {
            return null;
        }

        final String lotTitleSelector = "h4:containsOwn(Sklop)";
        final String sectionII21TitleSelector = "h5:containsOwn(II.2.1" + ENarocanjeTenderParser.NBSP_CHARACTER
                + "Naslov)";
        final String sectionII22TitleSelector = "h5:containsOwn(II.2.2" + ENarocanjeTenderParser.NBSP_CHARACTER
                + "Dodatna(-e) koda(-e) CPV)";
        final String sectionII22TableSelector = sectionII22TitleSelector + " + table";
        final String sectionII26TitleSelector = "h5:containsOwn(II.2.6" + ENarocanjeTenderParser.NBSP_CHARACTER
                + "Ocenjena vrednost)";
        final String sectionII27TitleSelector = "h5:containsOwn(II.2.7" + ENarocanjeTenderParser.NBSP_CHARACTER
                + "Trajanje naročila, okvirnega sporazuma ali dinamičnega nabavnega sistema)";
        final String sectionII29TitleSelector = "h5:containsOwn(II.2.9" + ENarocanjeTenderParser.NBSP_CHARACTER
                + "Informacije o omejitvah števila kandidatov, ki bodo povabljeni k sodelovanju)";
        final String sectionII213TitleSelector = "h5:containsOwn(II.2.13" + ENarocanjeTenderParser.NBSP_CHARACTER
                + "Informacije o sredstvih EU)";
        final String sectionII214TitleSelector = "h5:containsOwn(II.2.14" + ENarocanjeTenderParser.NBSP_CHARACTER
                + "Dodatne informacije)";

        List<ParsedTenderLot> lots = new ArrayList<>();

        for (Element lotElement : lotElements) {
            final Element sectionII21 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionII21TitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionII22TitleSelector, lotElement));
            final Element sectionII26 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionII26TitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionII27TitleSelector, lotElement));
            // last node of section II.2.7 is title of II.2.9, because I have not found section II.2.8
            final Element sectionII27 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionII27TitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionII29TitleSelector, lotElement));
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
                                    ParserUtils.getFromContent(sectionII213, null,
                                            ENarocanjeTenderFormUtils.IS_EU_FUND_TITLE)))))
                    .setEstimatedDurationInDays(ParserUtils.getFromContent(sectionII27, null, "Trajanje v dnevih:"))
                    .setEstimatedDurationInMonths(ParserUtils.getFromContent(sectionII27, null, "Trajanje v mesecih:"));

            lots.add(lot);
        }

        return lots;
    }

    /**
     * Parses buyer info.
     *
     * @param sectionI1
     *         html for section I.1 (with buyer info)
     * @param sectionI4
     *         html for section I.4 (with buyer info)
     *
     * @return parsed buyer info
     */
    private static ParsedBody parseBuyer(final Element sectionI1, final Element sectionI4) {
        final ParsedBody buyer = parseCommonBodyAttributes(sectionI1);

        buyer.getAddress()
                .setUrl(JsoupUtils.selectText("label:containsOwn(Glavni naslov (URL):) + a", sectionI1));
        return buyer
                .setBuyerType(sectionI4 == null ? null : sectionI4.ownText());
    }

}
