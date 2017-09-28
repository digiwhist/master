package eu.digiwhist.worker.hr.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;
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
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.CHECKBOX_TEXT_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_IV_1_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_I_4_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_VI_4_SELECTOR;

/**
 * Handler for Croatian new publication data.
 *
 * @author Marek Mikes
 */
final class EOJNTenderNewHandler {
    private static final Logger logger = LoggerFactory.getLogger(EOJNTenderNewHandler.class);

    private static final String SUBSECTION_II_1_SELECTOR = "p:contains(II.1) + p + table";
    private static final String SUBSECTION_II_2_SELECTOR = "table:has(tbody > tr > td > p:contains(II.2))";
    private static final String SECTION_V_SELECTOR = "p:contains(Odjeljak V: Dodjela ugovora) + p + table";

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
        final Element subsectionVI4 = JsoupUtils.selectFirst(SUBSECTION_VI_4_SELECTOR, document);

        parsedTender
                .setPublications(parsePublications(document, url))
                .addBuyer(new ParsedBody()
                        .setName(JsoupUtils.selectFirst("input[name=KorisnikNaziv1]", document).val())
                        .addBodyId(new BodyIdentifier()
                                .setId(JsoupUtils.selectFirst("input[name=KorisnikOIB1]", document).val()))
                        .setAddress(new ParsedAddress()
                                .setStreet(JsoupUtils.selectFirst("input[name=KorisnikUlicaIBroj1]", document).val())
                                .setCity(JsoupUtils.selectFirst("input[name=KorisnikGrad1]", document).val())
                                .setPostcode(JsoupUtils.selectFirst("input[name=KorisnikPbr1]", document).val())
                                .setCountry(JsoupUtils.selectFirst("input[name=KorisnikDrzava1]", document).val())
                                .setUrl(JsoupUtils.selectFirst("input[name=KontGlavnaStranica1]", document).val()))
                        .setContactName(JsoupUtils.selectFirst("input[name=ImeiPrezrezime1]", document).val())
                        .setPhone(JsoupUtils.selectFirst("input[name=TelOZZP1]", document).val())
                        .setEmail(JsoupUtils.selectFirst("input[name=EMailOZZP1]", document).val())
                        .setBuyerType(JsoupUtils.selectText(CHECKBOX_TEXT_SELECTOR, subsectionI4)))
                .setTitle(JsoupUtils.selectFirst("input[name=NazivNadmetanja1]", document).val())
                .setSupplyType(JsoupUtils.selectText(
                        "tr:has(input[name=VrstaPredmeta_RAD1]) > td > p > input[checked] + span", subsectionII1))
                .setIsFrameworkAgreement(JsoupUtils.exists("input[name=Poziv_OS1][checked]", subsectionIV1).toString())
                .setIsDps(JsoupUtils.exists("input[name=Postupak_DPS1][checked]", subsectionIV1).toString())
                .setIsElectronicAuction(JsoupUtils.exists("input[name=ElekDrazba_DA1]", subsectionIV1).toString())
                .setIsCentralProcurement(JsoupUtils.exists("input[name=UgDodSredNab_DA1][checked]", subsectionII1)
                        .toString())
                .setHasLots(EOJNTenderOldAndNewFormUtils.parseIfTenderHasLots(subsectionII1))
                .setDescription(JsoupUtils.selectFirst("input[name=KOPNP1]", document).val())
                .addCpv(new ParsedCPV()
                        .setIsMain(Boolean.TRUE.toString())
                        .setCode(JsoupUtils.selectFirst("input[name=CpvOznaka1]", subsectionII1).val()))
                .setFinalPrice(parseTenderFinalPrice(subsectionII1))
                .setIsCoveredByGpa(EOJNTenderOldAndNewFormUtils.parseIsTenderCoveredByGpa(subsectionIV1))
                .setProcedureType(JsoupUtils.selectText("td:contains(IV.1.1) input[checked] + span", subsectionIV1))
                .setBuyerAssignedId(JsoupUtils.selectFirst("input[name=EvidBrNab1]", document).val())
                .setLots(parseLots(document))
                .setAppealBodyName(EOJNTenderOldAndNewFormUtils.parseTenderAppealBodyName(subsectionVI4));

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
            case "Obavijest o dodjeli ugovora":
                updatedParsedTender = EOJNTenderNewContractAwardHandler1.parse(updatedParsedTender, document);
                break;
            case "Obavijest o dodjeli ugovora – sektorska nabava":
                updatedParsedTender = EOJNTenderNewContractAwardHandler2.parse(updatedParsedTender, document);
                break;
            default:
                logger.warn("No specific handler found for form type: \"{}\"", sourceFormType);
                break;
        }

        return updatedParsedTender;
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
                .setSourceFormType(EOJNTenderOldAndNewFormUtils.parsePublicationSourceFormType(document))
                .setSourceId(EOJNTenderOldAndNewFormUtils.parsePublicationSourceId(document))
                .setDispatchDate(JsoupUtils.selectFirst("input[name=DatumSlanjaDoc1]", document).val()));

        // previous publications
        if (!JsoupUtils.selectFirst("input[name=ORT_NAB_F2_N1]", document).val().isEmpty()) {
            throw new UnrecoverableException("Previous publication is filled -> refactor.");
        }

        return publications;
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
                .setCurrency(JsoupUtils.selectFirst("input[name=ValPon1]", subsection).val());

        final Boolean isPriceWithoutVat = JsoupUtils.exists("td:has(input[name=ValPon1]) > p:contains(bez PDV-a)",
                subsection);
        if (!isPriceWithoutVat) {
            throw new UnrecoverableException("Tender final price is with VAT -> refactor!");
        }

        final String amount = JsoupUtils.selectFirst("a[name=UkVrijUgBezPDV1] + span", subsection).val();
//        if (isPriceWithoutVat) {
            return price
                    .setNetAmount(amount);
//        } else {
//            return price
//                    .setAmountWithVat(amount)
//                    .setVat(JsoupUtils.selectText("a[name=PDVStopa1] + span", subsection));
//        }
    }

    /**
     * Parses all the lots from contract award notice.
     *
     * @param document
     *         document to be parsed
     *
     * @return list of all parsed lots or empty list if no lots specified
     */
    static List<ParsedTenderLot> parseLots(final Document document) {
        List<ParsedTenderLot> lots = new ArrayList<>();

        // get lots from subsection II.2
        final Element subsectionII2 = JsoupUtils.selectFirst(SUBSECTION_II_2_SELECTOR, document);
        Elements lotII2Elements = JsoupUtils.select("tr", subsectionII2);
        if (lotII2Elements == null) {
            return null;
        }
        if (lotII2Elements.size() > 1) {
            throw new UnrecoverableException("Number of lots is more than 1 -> refactor!");
        }
        for (Element lotII2Element : lotII2Elements) {
            lots.add(new ParsedTenderLot()
                    .setTitle(JsoupUtils.selectFirst("input[name=GrupePredmeta1]", lotII2Element).val())
                    .setLotNumber(JsoupUtils.selectFirst("input[name=GruRowNum1]", lotII2Element).val())
                    .setCpvs(parseLotCpvs(lotII2Element))
                    .setAddressOfImplementation(parseLotAddressOfImplementation(lotII2Element))
                    .setDescription(JsoupUtils.selectFirst("input[name=KratakOpisGr1]", lotII2Element).val())
                    .setAwardCriteria(parseLotAwardCriteria(lotII2Element))
                    .setHasOptions(EOJNTenderOldAndNewFormUtils.parseBooleanFromCheckboxes("OpcijeG_Da1", "OpcijeG_Ne1",
                            lotII2Element))
                    .addFunding(new ParsedFunding()
                            .setIsEuFund(EOJNTenderOldAndNewFormUtils.parseBooleanFromCheckboxes("NabPoPlanuEUG_Da1",
                                    "NabPoPlanuEUG_Ne1", lotII2Element))));
        }

        // merge lots from subsection II.2 with information from section V
        final Element sectionV = JsoupUtils.selectFirst(SECTION_V_SELECTOR, document);
        Elements lotVElements = JsoupUtils.select("tr", sectionV);
        if (lotVElements == null) {
            return lots;
        }
        if (lotVElements.size() > 1) {
            throw new UnrecoverableException("Number of lots is more than 1 -> refactor!");
        }
        for (Element lotVElement : lotVElements) {
            final String lotNumber = JsoupUtils.selectFirst("input[name=UgGrupaBr1]", lotVElement).val();
            final ParsedTenderLot lotFromII2 = lots
                    .stream()
                    .filter(l -> l.getLotNumber().equals(lotNumber))
                    .findFirst()
                    .orElse(null);
            assert lotFromII2 != null;

            final String currency = JsoupUtils.selectFirst("input[name=UgValuta1]", lotVElement).val();
            lotFromII2
                    .setContractNumber(JsoupUtils.selectFirst("input[name=UgovorOznaka1]", lotVElement).val())
                    .setIsAwarded(EOJNTenderOldAndNewFormUtils.parseBooleanFromCheckboxes("T_DodIliPonNZ_Da1",
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
                            .setIsSme(EOJNTenderOldAndNewFormUtils.parseBooleanFromCheckboxes("UGMaloSrednje_DA1",
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
     * Parses CPV codes for contract notice lot.
     *
     * @param lotElement
     *         element to be parsed
     *
     * @return list of parsed CPV codes
     */
    private static List<ParsedCPV> parseLotCpvs(final Element lotElement) {
        final List<ParsedCPV> cpvs = new ArrayList<>();

        // main CPV
        final List<String> mainCpvInputNames = Arrays.asList("CPVGrOz1", "DodPredGlCpvGrAN1", "DodPredGlCpvGrBN1",
                "DodPredGlCpvGrCN1", "DodPredGlCpvGrDN1");
        mainCpvInputNames
                .stream()
                .forEach(id -> {
                    final String code = JsoupUtils.selectFirst("input[name=" + id + "]", lotElement).val();
                    if (!code.isEmpty()) {
                        cpvs.add(new ParsedCPV()
                                .setIsMain(Boolean.TRUE.toString())
                                .setCode(code));
                    }
                });

        // other CPVs
        final List<String> otherCpvInputNames = Arrays.asList("CPVGrDodOznakaN1", "DodPredDodCpvGrAN1",
                "DodPredDodCpvGrBN1", "DodPredDodCpvGrCN1", "DodPredDodCpvGrDN1");
        otherCpvInputNames
                .stream()
                .forEach(id -> {
                    final String code = JsoupUtils.selectFirst("input[name=" + id + "]", lotElement).val();
                    if (!code.isEmpty()) {
                        cpvs.add(new ParsedCPV()
                                .setIsMain(Boolean.FALSE.toString())
                                .setCode(code));
                    }
                });

        return cpvs;
    }

    /**
     * Parses lot address of implementation.
     *
     * @param lotElement
     *         element to be parsed
     *
     * @return lot address of implementation
     */
    static ParsedAddress parseLotAddressOfImplementation(final Element lotElement) {
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
     * Parses lot award criteria.
     *
     * @param lotElement
     *         element to be parsed
     *
     * @return lot award criteria
     */
    static List<ParsedAwardCriterion> parseLotAwardCriteria(final Element lotElement) {
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

}
