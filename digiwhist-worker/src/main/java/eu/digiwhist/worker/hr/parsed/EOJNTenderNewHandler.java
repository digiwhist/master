package eu.digiwhist.worker.hr.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.CHECKBOX_TEXT_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.SECTION_V_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_II_1_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_II_2_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_IV_1_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_IV_2_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_IV_3_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_I_2_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_I_4_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_VI_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.TEXT_AFTER_TITLE_SELECTOR_PATTERN;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.parseBooleanFromCheckboxes;

/**
 * Handler for Croatian new publication data.
 *
 * @author Marek Mikes
 */
final class EOJNTenderNewHandler {
    private static final Logger logger = LoggerFactory.getLogger(EOJNTenderNewHandler.class);

    /**
     * This invisible character sometimes appears. We replace it manually, because trim function of String does not
     * remove it.
     */
    private static final Character CURIOUS_WHITE_SPACE = (char) 160;

    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderNewHandler() {
    }

    /**
     * Parse method for Croatian old publication data.
     *
     * @param document
     *         document to parse data from
     * @param url
     *         url of the document
     *
     * @return ParsedTender with data added
     */
    static ParsedTender parse(final Document document, final String url) {
        // common data are same for each document type
        ParsedTender parsedTender = parseCommonFormData(document, url);

        // form type specific
        parsedTender = parseFormSpecificData(parsedTender, document);

        return parsedTender;
    }

    /**
     * Parses common data and returns initialized parsed tender.
     *
     * @param document
     *         parsed document
     * @param url
     *         source data URL
     *
     * @return parsed tender
     */
    private static ParsedTender parseCommonFormData(final Document document, final String url) {
        final ParsedTender parsedTender = new ParsedTender();

        final Element subsectionI4 = JsoupUtils.selectFirst(SUBSECTION_I_4_SELECTOR, document);
        final Element subsectionII1 = JsoupUtils.selectFirst(SUBSECTION_II_1_SELECTOR, document);
        final Element subsectionIV1 = JsoupUtils.selectFirst(SUBSECTION_IV_1_SELECTOR, document);
        final Element subsectionIV2 = JsoupUtils.selectFirst(SUBSECTION_IV_2_SELECTOR, document);
        final Element subsectionIV3 = JsoupUtils.selectFirst(SUBSECTION_IV_3_SELECTOR, document);

        parsedTender
                .setPublications(parsePublications(document, url))
                .addBuyer(new ParsedBody()
                        .setName(parseFromSpanOrInput("KorisnikNaziv1", document))
                        .addBodyId(new BodyIdentifier()
                                .setId(parseFromSpanOrInput("KorisnikOIB1", document)))
                        .setAddress(new ParsedAddress()
                                .setStreet(parseFromSpanOrInput("KorisnikUlicaIBroj1", document))
                                .setCity(parseFromSpanOrInput("KorisnikGrad1", document))
                                .setPostcode(parseFromSpanOrInput("KorisnikPbr1", document))
                                .setCountry(parseFromSpanOrInput("KorisnikDrzava1", document))
                                .setUrl(parseFromSpanOrInput("KontGlavnaStranica1", document)))
                        .setContactName(parseFromSpanOrInput("ImeiPrezrezime1", document))
                        .setPhone(parseFromSpanOrInput("TelOZZP1", document))
                        .setEmail(parseFromSpanOrInput("EMailOZZP1", document))
                        .setBuyerType(parseBuyerType(document)))
                .setIsOnBehalfOf(JsoupUtils.exists("input[name=NabPoNaloguDru_NE1][checked]", subsectionI4)
                        .toString())
                .setTitle(parseTenderTitle(document))
                .setSupplyType(JsoupUtils.selectText(
                        "tr:has(input[name=VrstaPredmeta_RAD1]) > td > p > input[checked] + span" + ", "
                                + "tr:has(input[name=VrstaPredmeta_RAD1]) > td > p:has(input[checked])", subsectionII1))
                .setAddressOfImplementation(new ParsedAddress()
                        .setRawAddress(JsoupUtils.selectText(String.format(TEXT_AFTER_TITLE_SELECTOR_PATTERN,
                                "Glavno mjesto ili lokacija radova, isporuke robe ili pružanja usluga:"),
                                subsectionII1))
                        .addNuts(JsoupUtils.selectText("a[name=NUTS1] + span", subsectionII1)))
                .setIsFrameworkAgreement(JsoupUtils.exists("input[name=Poziv_OS1][checked]", document).toString())
                .setIsDps(JsoupUtils.exists("input[name=Postupak_DPS1][checked]", subsectionIV1).toString())
                .setIsElectronicAuction(parseIfTenderIsElectronicAuction(document))
                .setIsCentralProcurement(JsoupUtils.exists("input[name=UgDodSredNab_DA1][checked]", subsectionII1)
                        .toString())
                .setHasLots(parseIfTenderHasLots(subsectionII1))
                .setDescription(parseTenderDescription(document))
                .addCpv(new ParsedCPV()
                        .setIsMain(Boolean.TRUE.toString())
                        .setCode(parseFromSpanOrInput("CpvOznaka1", subsectionII1)))
                .setFinalPrice(parseTenderFinalPrice(document))
                .setIsCoveredByGpa(parseIsTenderCoveredByGpa(document))
                .setProcedureType(JsoupUtils.selectText("td:has(input[checked]) + td" + ", "
                        + "td:contains(IV.1.1) input[checked] + span", subsectionIV1))
                .setBuyerAssignedId(parseBuyerAssignedId(document))
                .setLots(parseLots(document))
                .setNpwpReasons(parseNpwpReasons(document))
                .addFunding(new ParsedFunding()
                        .setIsEuFund(parseBooleanFromCheckboxes("NabavaPoPlanuEU_DA1",
                                "NabavaPoPlanuEU_NE1", document)))
                .setSelectionMethod(parseTenderSelectionMethod(subsectionIV2))
                .setDocumentsPayable(parseBooleanFromCheckboxes("DokNapl_DA1",
                        "DokNapl_NE1", subsectionIV3))
                .setBidDeadline(JsoupUtils.selectText("a[name=RokZaDostavu1] + span", subsectionIV3) + ","
                        + JsoupUtils.selectText("a[name=RokZaDostavuSat1] + span", subsectionIV3))
                .addEligibleBidLanguage(JsoupUtils.selectText(
                        "tr:contains(IV.3.6) ~ tr:has(td:first-child input[checked]) > td:last-child", subsectionIV3))
                .setAwardDeadlineDuration(JsoupUtils.selectText("a[name=RokValjPonD1] + span", subsectionIV3));

        return parsedTender;
    }

    /**
     * Parses form specific data.
     *
     * @param parsedTender
     *         parsed tender
     * @param document
     *         parsed document
     *
     * @return parsed tender
     */
    private static ParsedTender parseFormSpecificData(final ParsedTender parsedTender, final Document document) {
        ParsedTender updatedParsedTender = parsedTender;

        final String sourceFormType = parsedTender.getPublications().get(0).getSourceFormType();
        switch (sourceFormType) {
            case "Obavijest o nadmetanju":
                updatedParsedTender = EOJNTenderNewContractNoticeHandler1.parse(updatedParsedTender, document);
                break;
            case "Poziv na nadmetanje":
                updatedParsedTender = EOJNTenderNewContractNoticeHandler2.parse(updatedParsedTender, document);
                break;
            case "Poziv na nadmetanje – sektor":
                updatedParsedTender = EOJNTenderNewContractNoticeHandler3.parse(updatedParsedTender, document);
                break;
            case "Obavijest o dodjeli ugovora":
                updatedParsedTender = EOJNTenderNewContractAwardHandler1.parse(updatedParsedTender, document);
                break;
            case "Obavijest o dodjeli ugovora – sektorska nabava":
                updatedParsedTender = EOJNTenderNewContractAwardHandler2.parse(updatedParsedTender, document);
                break;
            case "Obavijest o sklopljenim ugovorima":
                updatedParsedTender = EOJNTenderNewContractAwardHandler3.parse(updatedParsedTender, document);
                break;
            case "Obavijest o sklopljenim ugovorima - sektor":
                updatedParsedTender = EOJNTenderNewContractAwardHandler4.parse(updatedParsedTender, document);
                break;
            default:
                logger.warn("No specific handler found for form type: \"{}\"", sourceFormType);
                break;
        }

        return updatedParsedTender;
    }

    /**
     * Parses text value from span element or input element according to name of the element.
     *
     * @param elementName
     *         name of the element
     * @param element
     *         element to be parsed
     *
     * @return text value of desired element or null
     */
    private static String parseFromSpanOrInput(final String elementName, final Element element) {
        final String spanText = JsoupUtils.selectText("a[name=" + elementName + "] + span", element);
        if (spanText != null) {
            return spanText;
        }

        final Element inputElement = JsoupUtils.selectFirst("input[name=" + elementName + "]", element);
        if (inputElement != null) {
            return inputElement.val();
        }

        return null;
    }

    /**
     * Parses info about this publication and all the related and previous ones.
     *
     * @param document
     *         parsed document tree for the source HTML page
     * @param url
     *         source data URL
     *
     * @return list of all parsed publications including previous ones and other related ones
     */
    private static List<ParsedPublication> parsePublications(final Document document,
                                                             final String url) {
        final List<ParsedPublication> publications = new ArrayList<>();

        // this publication
        publications.add(new ParsedPublication()
                .setIsIncluded(true)
                .setHumanReadableUrl(url)
                .setSource(PublicationSources.HR_EOJN)
                .setSourceFormType(parsePublicationSourceFormType(document))
                .setSourceId(parsePublicationSourceId(document))
                .setDispatchDate(parsePublicationDispatchDate(document)));

        // previous publications
        publications.addAll(parsePreviousPublications(document));

        return publications;
    }

    /**
     * Parses information about previous publications (usually in TED) related to
     * the same tender. This information is typically provided in the IV.3.2
     * subsection.
     *
     * @param document
     *         parsed document tree for the source HTML page
     *
     * @return list of previous publications related to the same tender or empty
     * list if no such publications found
     */
    private static List<ParsedPublication> parsePreviousPublications(final Document document) {
        final List<ParsedPublication> publications = new ArrayList<>();

        // previous publications of newer publications.
        // Input names of elements are different:
        // - "OOT_NAB_F5_N1" - see
        //   https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1114400
        // - "ORT_NAB_F21_N1" - see
        //   https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1109404
        // - "RanijaObjaObav1" - see
        //   https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1147215
        final Element previousPublicationSourceIdElement = JsoupUtils.selectFirst(
                "input[name=ORT_NAB_F2_N1], input[name=ORT_NAB_F21_N1], input[name=OOT_NAB_F5_N1], " +
                        "input[name=RanijaObjaObav1]", document);
        if (previousPublicationSourceIdElement != null && !previousPublicationSourceIdElement.val().isEmpty()) {
            // parse only source ID of previous publication. See
            // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1113928
            publications.add(new ParsedPublication()
                    .setIsIncluded(false)
                    .setSource(PublicationSources.HR_EOJN)
                    .setSourceId(previousPublicationSourceIdElement.val()));
        }

        final Element subsectionIV3 = JsoupUtils.selectFirst(SUBSECTION_IV_3_SELECTOR, document);

        // "RanijaObj_Da1" is in CA and "RanijaObjava_DA1" is in CFT
        final Boolean existPreviousPublications = JsoupUtils.exists(
                "input[name=RanijaObj_Da1][checked], input[name=RanijaObjava_DA1][checked]", subsectionIV3);
        if (!existPreviousPublications) {
            return publications;
        }

        // we get source form type from the whole row, because sometimes it is in span and sometimes it is in td. See
        // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1141234 (in td)

        // invitation to tender
        if (JsoupUtils.exists("input[name=PredObjPNN1][checked]", subsectionIV3)) {
            publications.add(new ParsedPublication()
                    .setIsIncluded(false)
                    .setSource(PublicationSources.HR_EOJN)
                    .setSourceFormType(JsoupUtils.selectText("tr:has(input[name=PredObjPNN1])", subsectionIV3)
                            .replace(CURIOUS_WHITE_SPACE, ' ').trim())
                    .setSourceId(JsoupUtils.selectText("a[name=PredObjPNNOzn1] + span", subsectionIV3))
                    .setPublicationDate(JsoupUtils.selectText("a[name=PredObjPNNDat1] + span", subsectionIV3)));
        }

        // other previous publications
        if (JsoupUtils.exists("input[name=RanijaObjDrug_DA1][checked]", subsectionIV3)) {
            publications.add(new ParsedPublication()
                    .setIsIncluded(false)
                    .setSource(PublicationSources.HR_EOJN)
                    .setSourceFormType(JsoupUtils.selectText("tr:has(input[name=RanijaObjDrug_DA1])", subsectionIV3)
                            .replace(CURIOUS_WHITE_SPACE, ' ').trim())
                    .setSourceId(JsoupUtils.selectText("a[name=RanObjDrugBR1] + span", subsectionIV3))
                    .setPublicationDate(JsoupUtils.selectText("a[name=RanObjDrugDat1] + span", subsectionIV3)));
        }

        if (publications.isEmpty()) {
            // there should be some previous publications, but we parsed nothing. It can happen - see
            // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=175165
            logger.warn("There should be some previous publications, but we parsed nothing.");
        }

        return publications;
    }

    /**
     * Parse tender final price value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return tender final price or Null
     */
    private static ParsedPrice parseTenderFinalPrice(final Document document) {
        final Element subsectionII1 = JsoupUtils.selectFirst(SUBSECTION_II_1_SELECTOR, document);
        final Element subsectionII2 = JsoupUtils.selectFirst(SUBSECTION_II_2_SELECTOR, document);

        Element currencyElement = JsoupUtils.selectFirst("input[name=ValPon1]", subsectionII1);
        if (currencyElement == null) {
            currencyElement = JsoupUtils.selectFirst("input[name=Valuta1]", subsectionII2);
        }
        if (currencyElement == null) {
            // publication does not have any final price. See
            // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1145375
            return null;
        }

        ParsedPrice price = new ParsedPrice()
                .setCurrency(currencyElement.val());

        Boolean isPriceWithoutVat = JsoupUtils.exists("td:has(input[name=ValPon1]) > p:contains(bez PDV-a)",
                subsectionII1);
        if (!isPriceWithoutVat) {
            isPriceWithoutVat = JsoupUtils.exists("td:has(input[name=Valuta1]) + td > p > input[checked]",
                    subsectionII2);
        }

        String amount = JsoupUtils.selectText("a[name=UkVrijUgSaPDV1] + span", subsectionII2);
        if (amount == null) {
            // amount is probably in input element. See
            // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1095769
            final Element amountElement = JsoupUtils.selectFirst("input[name=UkVrijUgBezPDV1]", subsectionII1);
            amount = amountElement == null ? null : amountElement.val();
        }

        if (isPriceWithoutVat) {
            return price
                    .setNetAmount(amount);
        } else {
            return price
                    .setAmountWithVat(amount)
                    .setVat(JsoupUtils.selectText("a[name=PDVStopa1] + span", document));
        }
    }

    /**
     * Parses all the lots.
     *
     * @param document
     *         document to be parsed
     *
     * @return list of all parsed lots or empty list if no lots specified
     */
    private static List<ParsedTenderLot> parseLots(final Document document) {
        return JsoupUtils.exists("input[name=GrupePredmeta1]", document)
                ? parseNewerLots(document)
                : parseOlderLots(document);
    }

    /**
     * Parses all the lots of older form.
     *
     * @param document
     *         document to be parsed
     *
     * @return list of all parsed lots or empty list if no lots specified
     */
    private static List<ParsedTenderLot> parseOlderLots(final Document document) {
        final Element sectionV = JsoupUtils.selectFirst(SECTION_V_SELECTOR, document);
        Elements lotSections = JsoupUtils.select("table > tbody > tr:has(table)", sectionV);
        if (lotSections == null || lotSections.isEmpty()) {
            // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1145375
            return null;
        }

        List<ParsedTenderLot> lots = new ArrayList<>();

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
                        .setPrice(parseOlderLotFinalPrice(finalPriceRow, "p:has(a[name=UgVrijednost1]) + p > input",
                                "a[name=UgVrijednost1] + span", "a[name=PDVProcStopaUg1] + span"))
                        .setIsSubcontracted(JsoupUtils.exists("input[name=UgDajePodug_DA1][checked]", firstLotSection)
                                .toString()))
                .setEstimatedPrice(parseOlderLotEstimatedPrice(firstLotSection, finalPriceRow,
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
                            .setPrice(parseOlderLotFinalPrice(finalPriceRow, "td:nth-child(1) > p:nth-child(3) > input",
                                    "td:nth-child(1) > p:nth-child(2) > span:nth-child(3)",
                                    "td:nth-child(4) > p > span"))
                            .setIsSubcontracted(JsoupUtils.exists("input[name=UgDajePodug_DA1][checked]", lotSection)
                                    .toString()))
                    .setEstimatedPrice(parseOlderLotEstimatedPrice(lotSection, finalPriceRow,
                            "tr table tr:nth-child(10)",
                            "tr:nth-child(1) > td:nth-child(1) > p:nth-child(2) > span:nth-child(3)")));
        }

        return lots;
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
    private static ParsedPrice parseOlderLotFinalPrice(final Element finalPriceRow, final String currencySelector,
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
    private static ParsedPrice parseOlderLotEstimatedPrice(final Element lotSection, final Element finalPriceRow,
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
     * Parses all the lots of newer form.
     *
     * @param document
     *         document to be parsed
     *
     * @return list of all parsed lots or empty list if no lots specified
     */
    private static List<ParsedTenderLot> parseNewerLots(final Document document) {
        List<ParsedTenderLot> lots = new ArrayList<>();

        // Two lots are for example in
        // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1146597

        // get lots from subsection II.2

        final Element subsectionII2 = JsoupUtils.selectFirst(SUBSECTION_II_2_SELECTOR, document);
        if (subsectionII2 == null) {
            return null;
        }
        // we have to use ":root" to get rows, otherwise we get inner rows representing subsections sometimes. See
        // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1146594
        Elements lotII2Elements = subsectionII2.nodeName().equals("table")
                ? JsoupUtils.select(":root > tbody > tr", subsectionII2)
                // lot is in <tr> element. See
                // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1147215
                : new Elements(subsectionII2);
        for (Element lotII2Element : lotII2Elements) {
            lots.add(new ParsedTenderLot()
                    .setTitle(JsoupUtils.selectFirst("input[name=GrupePredmeta1]", lotII2Element).val())
                    .setLotNumber(JsoupUtils.selectFirst("input[name=GruRowNum1]", lotII2Element).val())
                    .setCpvs(parseNewerLotCpvs(lotII2Element))
                    .setAddressOfImplementation(parseNewerLotAddressOfImplementation(lotII2Element))
                    .setDescription(JsoupUtils.selectFirst("input[name=KratakOpisGr1]", lotII2Element).val())
                    .setAwardCriteria(parseNewerLotAwardCriteria(lotII2Element))
                    .setHasOptions(parseBooleanFromCheckboxes("OpcijeG_Da1", "OpcijeG_Ne1",
                            lotII2Element))
                    .addFunding(new ParsedFunding()
                            .setIsEuFund(parseBooleanFromCheckboxes("NabPoPlanuEUG_Da1",
                                    "NabPoPlanuEUG_Ne1", lotII2Element))));
        }

        // merge lots from subsection II.2 with information from section V

        final Element sectionV = JsoupUtils.selectFirst(SECTION_V_SELECTOR, document);
        Elements lotVElements = JsoupUtils.select(":root > tbody > tr", sectionV);
        if (lotVElements == null) {
            return lots;
        }
        for (Element lotVElement : lotVElements) {
            final String lotNumber = JsoupUtils.selectFirst("input[name=UgGrupaBr1]", lotVElement).val();
            final ParsedTenderLot lotFromII2 = lots
                    .stream()
                    .filter(l -> l.getLotNumber().equals(lotNumber))
                    .findFirst()
                    .orElse(null);
            if (lotFromII2 == null) {
                // e.g. https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1147215
                continue;
            }

            final String currency = JsoupUtils.selectFirst("input[name=UgValuta1]", lotVElement).val();
            lotFromII2
                    .setContractNumber(JsoupUtils.selectFirst("input[name=UgovorOznaka1]", lotVElement).val())
                    .setIsAwarded(parseBooleanFromCheckboxes("T_DodIliPonNZ_Da1",
                            "T_DodIliPonNZ_Ne1", lotVElement))
                    .setAwardDecisionDate(JsoupUtils.selectFirst("input[name=DatOdabUgGr1]", lotVElement).val())
                    .setBidsCount(JsoupUtils.selectFirst("input[name=BrZapPonGr1]", lotVElement).val())
                    .setSmeBidsCount(JsoupUtils.selectFirst("input[name=BrZapPonMSP1]", lotVElement).val())
                    .setElectronicBidsCount(JsoupUtils.selectFirst("input[name=BrZapElPonGr1]", lotVElement).val())
                    .addBid(new ParsedBid()
                            .setIsWinning(Boolean.TRUE.toString())
                            .addBidder(new ParsedBody()
                                    .setName(JsoupUtils.selectFirst("input[name=UGgStrNaziv1]", lotVElement).val())
                                    .addBodyId(new BodyIdentifier()
                                            .setId(JsoupUtils.selectFirst("input[name=UGgStrOIB1]", lotVElement).val()))
                                    .setEmail(JsoupUtils.selectFirst("input[name=UGgStrEMail1]", lotVElement).val())
                                    .setPhone(JsoupUtils.selectFirst("input[name=UGgStrTel1]", lotVElement).val())
                                    .setAddress(new ParsedAddress()
                                            .setUrl(JsoupUtils.selectFirst("input[name=UGgStrURL1]", lotVElement)
                                                    .val()))
                            .setIsSme(parseBooleanFromCheckboxes("UGMaloSrednje_DA1",
                                    "UGMaloSrednje_NE1", lotVElement)))
                            .setPrice(new ParsedPrice()
                                    .setNetAmount(JsoupUtils.selectFirst("input[name=UgVrijednost1]", lotVElement)
                                            .val())
                                    .setCurrency(currency))
                            .setIsSubcontracted(JsoupUtils.exists("input[name=UgDajePodugGr_Da1][checked]",
                                    lotVElement).toString()))
                    .setAddressOfImplementation(new ParsedAddress()
                            .setStreet(JsoupUtils.selectFirst("input[name=UGgStrPAdr1]", lotVElement).val())
                            .setCity(JsoupUtils.selectFirst("input[name=UGgStrMjesto1]", lotVElement).val())
                            .addNuts(JsoupUtils.selectFirst("input[name=UGgNUTSPon1]", lotVElement).val())
                            .setPostcode(JsoupUtils.selectFirst("input[name=UGgStrPbr1]", lotVElement).val())
                            .setCountry(JsoupUtils.selectFirst("input[name=UGgStrDrz1]", lotVElement).val()))
                    .setEstimatedPrice(new ParsedPrice()
                            .setNetAmount(JsoupUtils.selectFirst("input[name=UgGrupePredProc1]", lotVElement).val())
                            .setCurrency(currency));
        }

        return lots;
    }

    /**
     * Parses CPV codes for lot of newer form.
     *
     * @param lotElement
     *         element to be parsed
     *
     * @return list of parsed CPV codes
     */
    private static List<ParsedCPV> parseNewerLotCpvs(final Element lotElement) {
        final List<ParsedCPV> cpvs = new ArrayList<>();

        // main CPV
        final List<String> mainCpvInputNames = Arrays.asList(
                "CPVGrOz1",
                "DodPredGlCpvGrAN1", "DodPredGlCpvGrBN1", "DodPredGlCpvGrCN1", "DodPredGlCpvGrDN1",
                "DodPredGlCpvGrANU1", "DodPredGlCpvGrBNU1", "DodPredGlCpvGrCNU1", "DodPredGlCpvGrDNU1",
                "CPVUslugeGr1");
        mainCpvInputNames
                .stream()
                .forEach(id -> {
                    final Element codeElement = JsoupUtils.selectFirst("input[name=" + id + "]", lotElement);
                    if (codeElement != null && !codeElement.val().isEmpty()) {
                        cpvs.add(new ParsedCPV()
                                .setIsMain(Boolean.TRUE.toString())
                                .setCode(codeElement.val()));
                    }
                });

        // other CPVs
        final List<String> otherCpvInputNames = Arrays.asList(
                "CPVGrDodOznakaN1",
                "DodPredDodCpvGrAN1", "DodPredDodCpvGrBN1", "DodPredDodCpvGrCN1", "DodPredDodCpvGrDN1");
        otherCpvInputNames
                .stream()
                .forEach(id -> {
                    final Element codeElement = JsoupUtils.selectFirst("input[name=" + id + "]", lotElement);
                    if (codeElement != null && !codeElement.val().isEmpty()) {
                        cpvs.add(new ParsedCPV()
                                .setIsMain(Boolean.FALSE.toString())
                                .setCode(codeElement.val()));
                    }
                });

        return cpvs;
    }

    /**
     * Parses lot address of implementation of newer form.
     *
     * @param lotElement
     *         element to be parsed
     *
     * @return lot address of implementation
     */
    private static ParsedAddress parseNewerLotAddressOfImplementation(final Element lotElement) {
        List<String> nutsList = new ArrayList<>();
        final List<String> nutsInputNames = Arrays.asList("NUTSG1", "DodGNUTSA1", "DodGNUTSB1", "DodGNUTSC1",
                "DodGNUTSD1");
        nutsInputNames
                .stream()
                .forEach(id -> {
                    final String nuts = JsoupUtils.selectFirst("input[name=" + id + "]", lotElement).val();
                    if (!nuts.isEmpty()) {
                        nutsList.add(nuts);
                    }
                });

        return new ParsedAddress()
                .setNuts(!nutsList.isEmpty() ? null : nutsList)
                .setRawAddress(JsoupUtils.selectFirst("input[name=GlLokIzvrG1]", lotElement).val());
    }

    /**
     * Parses lot award criteria of newer form.
     *
     * @param lotElement
     *         element to be parsed
     *
     * @return lot award criteria
     */
    private static List<ParsedAwardCriterion> parseNewerLotAwardCriteria(final Element lotElement) {
        if (!JsoupUtils.exists("input[name=ENPKritGrUDZN_Ne1][checked]", lotElement)) {
            return null;
        }

        List<ParsedAwardCriterion> parsedCriteria = new ArrayList<>();

        // parse quality criteria
        if (JsoupUtils.exists("input[name=KritKval_Da1][checked]", lotElement)) {
            parsedCriteria.add(new ParsedAwardCriterion()
                    .setName(JsoupUtils.selectFirst("input[name=KritKvalRbr1]", lotElement).val())
                    .setWeight(JsoupUtils.selectFirst("input[name=KritKvalPonder1]", lotElement).val()));
        }

        // parse cost criteria
        if (JsoupUtils.exists("input[name=KritTros_Da1][checked]", lotElement)) {
            parsedCriteria.add(new ParsedAwardCriterion()
                    .setName(JsoupUtils.selectFirst("input[name=KritTrosRbr1]", lotElement).val())
                    .setWeight(JsoupUtils.selectFirst("input[name=KritTrosPonder1]", lotElement).val()));
        }

        // parse price criterion
        if (JsoupUtils.exists("input[name=KritCij_Da1][checked]", lotElement)) {
            parsedCriteria.add(new ParsedAwardCriterion()
                    .setName("PRICE")
                    .setWeight(JsoupUtils.selectFirst("input[name=CijenaPonder1]", lotElement).val())
                    .setIsPriceRelated(Boolean.TRUE.toString()));
        }

        return parsedCriteria;
    }

    /**
     * Returns the type of buyer.
     *
     * @param document
     *         document to be parsed
     *
     * @return type of the buyer
     */
    private static String parseBuyerType(final Document document) {
        final Element subsectionI2 = JsoupUtils.selectFirst(SUBSECTION_I_2_SELECTOR, document);
        String buyerType = JsoupUtils.selectText(CHECKBOX_TEXT_SELECTOR, subsectionI2);
        if (buyerType == null) {
            final Element subsectionI4 = JsoupUtils.selectFirst(SUBSECTION_I_4_SELECTOR, document);
            buyerType = JsoupUtils.selectText(CHECKBOX_TEXT_SELECTOR, subsectionI4);
        }
        return buyerType;
    }

    /**
     * Parse tender title from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderTitle(final Document document) {
        final Element titleInput = JsoupUtils.selectFirst("input[name=NazivNadmetanja1]", document);
        if (titleInput != null) {
            return titleInput.val();
        } else {
            final Element subsectionII1 = JsoupUtils.selectFirst(SUBSECTION_II_1_SELECTOR, document);
            return JsoupUtils.selectText(String.format(TEXT_AFTER_TITLE_SELECTOR_PATTERN, "II.1.1"), subsectionII1);
        }
    }

    /**
     * Parse tender buyer assigned Id value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseBuyerAssignedId(final Document document) {
        final Element buyerAssignedId = JsoupUtils.selectFirst("input[name=EvidBrNab1]", document);
        if (buyerAssignedId != null) {
            return buyerAssignedId.val();
        } else {
            final Element subsectionIV3 = JsoupUtils.selectFirst(SUBSECTION_IV_3_SELECTOR, document);
            return JsoupUtils.selectText(String.format(TEXT_AFTER_TITLE_SELECTOR_PATTERN, "IV.3.1"), subsectionIV3);
        }
    }

    /**
     * Parse tender description value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderDescription(final Document document) {
        // newer publications
        final Element descriptionElement = JsoupUtils.selectFirst("input[name=KOPNP1]", document);
        if (descriptionElement != null && !descriptionElement.val().isEmpty()) {
            return descriptionElement.val();
        }

        // older publications
        final Element subsectionII1 = JsoupUtils.selectFirst(SUBSECTION_II_1_SELECTOR, document);
        String description = JsoupUtils.selectCombinedText("p:contains(II.1.5) ~ p", subsectionII1);
        if (description == null) {
            description = JsoupUtils.selectCombinedText("p:contains(II.1.4) ~ p", subsectionII1);
        }
        return description;
    }

    /**
     * Parse tender selection method value from document.
     *
     * @param subsectionIV2Element
     *         subsection IV.2 to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderSelectionMethod(final Element subsectionIV2Element) {
        // the subsection contains other checkboxes, so we can not use selector to get general checkbox
        final String selectionMethod = JsoupUtils.selectText(
                "td:has(p > input[name=NajnizaCijena1][checked]) + td," +
                        "td:has(p > input[name=EkNajpPon1][checked]) + td",
                subsectionIV2Element);

        // the method can be unfilled.
        // See https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=203989

        return selectionMethod;
    }

    /**
     * Parse if tender is electronic auction value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private static String parseIfTenderIsElectronicAuction(final Document document) {
        final String tenderIsElectronicAuction = parseBooleanFromCheckboxes(
                "ElekDrazba_DA1", "ElekDrazba_NE1", document);
        return tenderIsElectronicAuction != null
                ? tenderIsElectronicAuction
                : parseBooleanFromCheckboxes("ElekDrazbaProv_DA1", "ElekDrazbaProv_NE1",
                document);
    }

    /**
     * Parse publication dispatch date value from document.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parsePublicationDispatchDate(final Document document) {
        Element dispatchDateElement = JsoupUtils.selectFirst("input[name=DatumSlanjaDoc1]", document);
        return dispatchDateElement != null
                ? dispatchDateElement.val()
                : parseFromSpanOrInput("DatSlanjaObjOOSUJ1", document);
    }

    /**
     * Parses source form type of publication.
     *
     * @param document
     *         parsed document
     *
     * @return publication source form type element
     */
    static String parsePublicationSourceFormType(final Document document) {
        Element sourceFormTypeElement = JsoupUtils.selectFirst(
                "p[style*='text-align:right']:has(span[style*='font-weight:bold'])", document);
        if (sourceFormTypeElement == null) {
            // e.g. https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1126476 or
            // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1111459
            sourceFormTypeElement = JsoupUtils.selectFirst(
                    "html > body > div:first-child + br + div > p:has(br:first-child)", document);
        }
        if (sourceFormTypeElement == null) {
            // e.g. https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=336535
            sourceFormTypeElement = JsoupUtils.selectFirst("html > body > div:first-child > p:first-child", document);
        }
        assert sourceFormTypeElement != null;

        String sourceFormType = sourceFormTypeElement.text().trim();

        if (sourceFormType.equals("Obavijest o dodatnim informacijama, poništenju postupka ili ispravku")) {
            // it is correction/cancellation. We have to append string from VI.1 to distinguish it. See
            // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1147189

            final Element subsectionVI = JsoupUtils.selectFirst(SUBSECTION_VI_SELECTOR, document);
            final Element subsectionVI1 = JsoupUtils.selectFirst(":root > tbody > tr:contains(VI.1) + tr",
                    subsectionVI);
            // get elements which distinguish the types. Sometimes we get nothing - see
            // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=481956
            final Elements typeElements = JsoupUtils.select(":root > td > p:has(input[checked]) > span:first-child",
                    subsectionVI1);
            StringBuilder stringBuilder = new StringBuilder().append(sourceFormType);
            typeElements.forEach(e -> stringBuilder.append(" - ").append(e.text()));
            sourceFormType = stringBuilder.toString();
        }

        return sourceFormType;
    }

    /**
     * Parses source ID value of publication from document.
     *
     * @param document
     *         parsed document tree for the source HTML page
     *
     * @return String or Null
     */
    static String parsePublicationSourceId(final Document document) {
        final String sourceIdTitle = "Broj objave:";
        // the Id is in span, but sometime it is in two spans, so we want parent paragraph "p". See
        // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=764694
        final Elements sourceIdElements = JsoupUtils.select(
                "html > body > div > p:contains(" + sourceIdTitle + ")", document);
        if (sourceIdElements.isEmpty()) {
            // some forms do not have the ID. See
            // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=186022
            return null;
        } else {
            // there can be many elements with "Broj objave:", but we want the last one. See
            // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=436666
            final String sourceIdParagraphText = sourceIdElements.last().text();
            if (sourceIdParagraphText.contains(sourceIdTitle)) {
                return sourceIdParagraphText.substring(sourceIdParagraphText.lastIndexOf(sourceIdTitle) +
                        sourceIdTitle.length()).trim();
            } else {
                // sometimes we find useless text, where the title of source ID is ambiguous. See
                // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=173225
                return null;
            }
        }
    }

    /**
     * Parse if tender is covered by GPA.
     *
     * @param document
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseIsTenderCoveredByGpa(final Document document) {
        return parseBooleanFromCheckboxes("WTOSporJNabava_DA1", "WTOSporJNabava_NE1", document);
    }

    /**
     * Parse if tender has lots value from document.
     *
     * @param subsectionII1
     *         subsection II.1 to be parsed
     *
     * @return String or Null
     */
    static String parseIfTenderHasLots(final Element subsectionII1) {
        return parseBooleanFromCheckboxes("GrupeDijPredNab_DA1", "GrupeDijPredNab_NE1", subsectionII1);
    }

    /**
     * Parses NPWP reasons from annex D.
     *
     * @param document
     *         document to be parsed
     *
     * @return list of NPWP reasons or null
     */
    static List<String> parseNpwpReasons(final Document document) {
        final List<String> npwpReasons = new ArrayList<>();

        // list of input names of reasons, where we can use just the span next the input element
        final List<String> simpleReasonInputNames = Arrays.asList(
                "NDZPost_OTVJ1", "NDZPost_OGRJ1",
                "IznimZur_DA1", "DodRadStogo_DA1", "NovRadStrogo_DA1", "UgIzNatjec_DA1",
                "Postupak_IIB1", "Postupak_Izuz1");
        simpleReasonInputNames
                .stream()
                .forEach(n -> {
                    final Element checkedReasonElement = JsoupUtils.selectFirst("input[name=" + n +
                            "][checked] + span + span", document);
                    if (checkedReasonElement != null) {
                        npwpReasons.add(checkedReasonElement.text());
                    }
                });

        // list of input names of reasons, which have sub-reasons. We join the reason with its sub-reason.
        // Both parts have to be checked, otherwise we do not add the reason. I hope that it is correct..
        Element firstPartOfCheckedReasonElement1 = JsoupUtils.selectFirst(
                "input[name=UgIstaz_DA1][checked] + span + span", document);
        if (firstPartOfCheckedReasonElement1 != null) {
            final List<String> secondPartOfReasonInputNames = Arrays.asList(
                    "OdrGSRazl_TEH1", "OdrGSRazl_UMJ1", "OdrGSRazl_ISKPR1");
            secondPartOfReasonInputNames
                    .stream()
                    .forEach(n -> {
                        final Element checkedSecondPartOfReasonElement = JsoupUtils.selectFirst("input[name=" + n +
                                "][checked] + span + span", document);
                        if (checkedSecondPartOfReasonElement != null) {
                            npwpReasons.add(firstPartOfCheckedReasonElement1.text() +
                                    checkedSecondPartOfReasonElement.text());
                        }
                    });
        }
        Element firstPartOfCheckedReasonElement2 = JsoupUtils.selectFirst(
                "input[name=RobaBurza_DA1][checked] + span + span", document);
        if (firstPartOfCheckedReasonElement2 != null) {
            final List<String> secondPartOfReasonInputNames = Arrays.asList("KupRPosUvj_OBPOSDJ1", "KupRPosUvj_STEC1");
            secondPartOfReasonInputNames
                    .stream()
                    .forEach(n -> {
                        final Element checkedSecondPartOfReasonElement = JsoupUtils.selectFirst("input[name=" + n +
                                "][checked] + span + span", document);
                        if (checkedSecondPartOfReasonElement != null) {
                            npwpReasons.add(firstPartOfCheckedReasonElement2.text() +
                                    checkedSecondPartOfReasonElement.text());
                        }
                    });
        }

        return npwpReasons;
    }

}
