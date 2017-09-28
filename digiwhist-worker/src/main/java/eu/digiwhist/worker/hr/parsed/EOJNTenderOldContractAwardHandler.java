package eu.digiwhist.worker.hr.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.List;

import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldFormUtils.SUBSECTION_II_1_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldFormUtils.SUBSECTION_II_2_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SECTION_SELECTOR_PATTERN;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_IV_2_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_VI_3_SELECTOR;

/**
 * Old contract award form parser for Croatia.
 *
 * @author Marek Mikes
 */
final class EOJNTenderOldContractAwardHandler {
    static final String SECTION_V_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "Odjeljak V: Sklapanje ugovora");

    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderOldContractAwardHandler() {}

    /**
     * Parses data for old contract award form.
     *
     * @param parsedTender
     *         tender to add data to
     * @param document
     *         document to parse data from
     *
     * @return ParsedTender with parsed data
     */
    static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        final Element subsectionII1 = JsoupUtils.selectFirst(SUBSECTION_II_1_SELECTOR, document);
        final Element subsectionII2 = JsoupUtils.selectFirst(SUBSECTION_II_2_SELECTOR, document);
        final Element subsectionIV2 = JsoupUtils.selectFirst(SUBSECTION_IV_2_SELECTOR, document);
        final Element sectionV = JsoupUtils.selectFirst(SECTION_V_SELECTOR, document);
        final Element subsectionVI3 = JsoupUtils.selectFirst(SUBSECTION_VI_3_SELECTOR, document);

        parsedTender
                .setDescription(EOJNTenderOldFormUtils.parseTenderDescription(subsectionII1, "II.1.4"))
                .setFinalPrice(parseTenderFinalPrice(subsectionII2))
                .setSelectionMethod(EOJNTenderOldFormUtils.parseTenderSelectionMethod(subsectionIV2))
                .setIsElectronicAuction(EOJNTenderOldAndNewFormUtils.parseBooleanFromCheckboxes("ElekDrazbaProv_DA1",
                        "ElekDrazbaProv_NE1", subsectionIV2))
                .setLots(parseLots(sectionV))
                .setAppealBodyName(EOJNTenderOldAndNewFormUtils.parseTenderAppealBodyName(subsectionVI3));

        parsedTender.getPublications().get(0)
                .setDispatchDate(JsoupUtils.selectText("a[name=DatSlanjaObjOOSUJ1] + span", document));

        return parsedTender;
    }

    /**
     * Parse tender final price value from document.
     *
     * @param subsection
     *         subsection to be parsed
     *
     * @return tender final price or Null
     */
    private static ParsedPrice parseTenderFinalPrice(final Element subsection) {
        ParsedPrice price = new ParsedPrice()
                .setCurrency(JsoupUtils.selectFirst("input[name=Valuta1]", subsection).val());

        final Boolean isPriceWithoutVat = JsoupUtils.exists("td:has(input[name=Valuta1]) + td > p > input[checked]",
                subsection);
        assert isPriceWithoutVat || JsoupUtils.exists("td:has(input[name=Valuta1]) + td + td > p > input[checked]",
                subsection) : "Price has to be with or without VAT!";

        final String amount = JsoupUtils.selectText("a[name=UkVrijUgSaPDV1] + span", subsection);
        if (isPriceWithoutVat) {
            return price
                    .setNetAmount(amount);
        } else {
            return price
                    .setAmountWithVat(amount)
                    .setVat(JsoupUtils.selectText("a[name=PDVStopa1] + span", subsection));
        }
    }

    /**
     * Parses lot estimated price.
     *
     * @param lotSection
     *         lot html
     * @param finalPriceRow
     *         final price in html row
     *         It is useful, because we get wrapped two rows where the price is
     * @param firstRowOfEstimatedPriceSelector
     *         selector of first row of estimated price.
     *         It is useful, because we get wrapped two rows where the price is
     * @param amountSelector
     *         selector of amount
     *
     * @return lot estimated price
     */
    private static ParsedPrice parseLotEstimatedPrice(final Element lotSection, final Element finalPriceRow,
                                                      final String firstRowOfEstimatedPriceSelector,
                                                      final String amountSelector) {
        final Element estimatedPriceSection = ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(firstRowOfEstimatedPriceSelector, lotSection),
                finalPriceRow);

        final ParsedPrice price = new ParsedPrice()
                .setCurrency(JsoupUtils.selectText("tr:first-child > td:first-child > p:last-child > span:last-child",
                        estimatedPriceSection));

        final Boolean isPriceWithoutVat = JsoupUtils.exists(
                "tr:last-child > td:first-child > p > input[checked]", estimatedPriceSection);
        assert isPriceWithoutVat || JsoupUtils.exists("tr:last-child > td:nth-child(2) > p > input[checked]",
                estimatedPriceSection) : "Price has to be with or without VAT!";

        final String amount = JsoupUtils.selectText(amountSelector, estimatedPriceSection);
        if (isPriceWithoutVat) {
            return price
                    .setNetAmount(amount);
        } else {
            return price
                    .setAmountWithVat(amount)
                    .setVat(JsoupUtils.selectText("tr:last-child > td:last-child > p > span", estimatedPriceSection));
        }
    }

    /**
     * Parses lot final price. HTML structure of first lot differs from second, third,.. lots, so most of selectors
     * differ too.
     *
     * @param finalPriceRow
     *         final price in html row
     * @param currencySelector
     *         selector of currency
     * @param amountSelector
     *         selector of amount
     * @param vatSelector
     *         selector of VAT
     *
     * @return lot final price
     */
    private static ParsedPrice parseLotFinalPrice(final Element finalPriceRow, final String currencySelector,
                                                  final String amountSelector, final String vatSelector) {
        final ParsedPrice price = new ParsedPrice()
                .setCurrency(JsoupUtils.selectFirst(currencySelector, finalPriceRow).val());

        final Boolean isPriceWithoutVat = JsoupUtils.exists("tr > td:nth-child(2) > p > input[checked]", finalPriceRow);
        assert isPriceWithoutVat || JsoupUtils.exists("tr > td:nth-child(3) > p > input[checked]", finalPriceRow)
                : "Price has to be with or without VAT!";

        final String amount = JsoupUtils.selectText(amountSelector, finalPriceRow);
        if (isPriceWithoutVat) {
            return price
                    .setNetAmount(amount);
        } else {
            return price
                    .setAmountWithVat(amount)
                    .setVat(JsoupUtils.selectText(vatSelector, finalPriceRow));
        }
    }

    /**
     * Parses all the lots from contract award notice.
     *
     * @param sectionV
     *         section V to be parsed
     *
     * @return list of all parsed lots or empty list if no lots specified
     */
    static List<ParsedTenderLot> parseLots(final Element sectionV) {
        List<ParsedTenderLot> lots = new ArrayList<>();

        Elements lotSections = JsoupUtils.select("table > tbody > tr:has(table)", sectionV);
        assert !lotSections.isEmpty();

        // it is strange, but HTML subsection of first lot differs from other lot subsections

        // first lot:
        Element finalPriceRow = JsoupUtils.selectFirst("tr tr:has(a[name=UgVrijednost1])", lotSections.get(0));
        final Element firstLotSection = lotSections.get(0);
        lots.add(new ParsedTenderLot()
                .setContractNumber(JsoupUtils.selectText("a[name=UgovorOznaka1] + span", firstLotSection))
                .setLotNumber(JsoupUtils.selectText("a[name=UgGrupaBr1] + span", firstLotSection))
                .setTitle(JsoupUtils.selectText("a[name=UgGrupaNaz1] + span", firstLotSection))
                .setAwardDecisionDate(JsoupUtils.selectText("a[name=DatOdabUg1] + span", firstLotSection))
                .setBidsCount(JsoupUtils.selectText("a[name=BrZapPon1] + span, a[name=BrZapPonGr1] + span",
                        firstLotSection))
                .setElectronicBidsCount(JsoupUtils.selectText("a[name=BrZapElPonGr1] + span", firstLotSection))
                .addBid(new ParsedBid()
                        .setIsWinning(Boolean.TRUE.toString())
                        .addBidder(new ParsedBody()
                                .setName(JsoupUtils.selectText("a[name=UGgStrNaziv1] + span", firstLotSection))
                                .setAddress(new ParsedAddress()
                                        .setStreet(JsoupUtils.selectText("a[name=UGgStrPAdr1] + span", firstLotSection))
                                        .setCity(JsoupUtils.selectText("a[name=UGgStrMjesto1] + span", firstLotSection))
                                        .setPostcode(JsoupUtils.selectText("a[name=UGgStrPbr1] + span",
                                                firstLotSection))
                                        .setCountry(JsoupUtils.selectText("a[name=UGgStrDrz1] + span", firstLotSection))
                                        .setUrl(JsoupUtils.selectText("a[name=UGgStrURL1] + span", firstLotSection)))
                                .setEmail(JsoupUtils.selectText("a[name=UGgStrEMail1] + span", firstLotSection))
                                .setPhone(JsoupUtils.selectText("a[name=UGgStrTel1] + span", firstLotSection)))
                        .setPrice(parseLotFinalPrice(finalPriceRow, "p:has(a[name=UgVrijednost1]) + p > input",
                                "a[name=UgVrijednost1] + span", "a[name=PDVProcStopaUg1] + span"))
                        .setIsSubcontracted(JsoupUtils.exists("input[name=UgDajePodug_DA1][checked]", firstLotSection)
                                .toString()))
                .setEstimatedPrice(parseLotEstimatedPrice(firstLotSection, finalPriceRow,
                        "tr tr:has(a[name=UgGrupePredProc1])", "a[name=UgGrupePredProc1] + span")));

        // other lots:
        lotSections.remove(0);
        for (Element lotSection : lotSections) {
            finalPriceRow = JsoupUtils.selectFirst("tr table tr:nth-child(12)", lotSection);
            lots.add(new ParsedTenderLot()
                    .setContractNumber(JsoupUtils.selectText("tr > td > p > span:nth-child(2)", lotSection))
                    .setLotNumber(JsoupUtils.selectText("tr > td > p > span:nth-child(5)", lotSection))
                    .setTitle(JsoupUtils.selectText("tr > td > p > span:nth-child(9)", lotSection))
                    .setAwardDecisionDate(JsoupUtils.selectText("tr table tr:nth-child(1) > td > p > span:nth-child(3)",
                            lotSection))
                    .setBidsCount(JsoupUtils.selectText(
                            "tr table tr:nth-child(2) > td > p:nth-child(2) > span:nth-child(3)", lotSection))
                    .setElectronicBidsCount(JsoupUtils.selectText(
                            "tr table tr:nth-child(2) > td > p:nth-child(3) > span:nth-child(3)", lotSection))
                    .addBid(new ParsedBid()
                            .setIsWinning(Boolean.TRUE.toString())
                            .addBidder(new ParsedBody()
                                    .setName(JsoupUtils.selectText(
                                            "tr table tr:nth-child(4) > td > p > span:nth-child(3)", lotSection))
                                    .setAddress(new ParsedAddress()
                                            .setStreet(JsoupUtils.selectText(
                                                    "tr table tr:nth-child(5) > td > p > span:nth-child(3)",
                                                    lotSection))
                                            .setCity(JsoupUtils.selectText("tr table tr:nth-child(6) > " +
                                                            "td:nth-child(1) > p > span:nth-child(3)", lotSection))
                                            .setPostcode(JsoupUtils.selectText(
                                                    "tr table tr:nth-child(6) > td:nth-child(2) > p > " +
                                                            "span:nth-child(3)", lotSection))
                                            .setCountry(JsoupUtils.selectText(
                                                    "tr table tr:nth-child(6) > td:nth-child(3) > p > " +
                                                            "span:nth-child(3)", lotSection))
                                            .setUrl(null)) // find form with many lots and filled URL
                                    .setEmail(JsoupUtils.selectText("tr table tr:nth-child(7) > td:nth-child(1) " +
                                            "> p > span:nth-child(3)", lotSection))
                                    .setPhone(JsoupUtils.selectText("tr table tr:nth-child(7) > td:nth-child(2) " +
                                            "> p > span:nth-child(3)", lotSection)))
                            .setPrice(parseLotFinalPrice(finalPriceRow, "td:nth-child(1) > p:nth-child(3) > input",
                                    "td:nth-child(1) > p:nth-child(2) > span:nth-child(3)",
                                    "td:nth-child(4) > p > span"))
                            .setIsSubcontracted(JsoupUtils.exists("input[name=UgDajePodug_DA1][checked]", lotSection)
                                    .toString()))
                    .setEstimatedPrice(parseLotEstimatedPrice(lotSection, finalPriceRow, "tr table tr:nth-child(10)",
                            "tr:nth-child(1) > td:nth-child(1) > p:nth-child(2) > span:nth-child(3)")));
        }

        return lots;
    }

}
