package eu.digiwhist.worker.hr.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldFormUtils.SUBSECTION_II_1_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.CHECKBOX_TEXT_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_IV_1_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_IV_3_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_I_1_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_I_2_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_I_3_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_I_4_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.TEXT_AFTER_TITLE_SELECTOR_PATTERN;

/**
 * Handler for Croatian old publication data.
 *
 * @author Marek Mikes
 */
final class EOJNTenderOldHandler {
    private static final Logger logger = LoggerFactory.getLogger(EOJNTenderOldHandler.class);

    /**
     * This invisible character sometimes appears. We replace it manually, because trim function of String does not
     * remove it.
     */
    private static final Character CURIOUS_WHITE_SPACE = (char) 160;

    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderOldHandler() {
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

        final Element subsectionI1 = JsoupUtils.selectFirst(SUBSECTION_I_1_SELECTOR, document);
        final Element subsectionI2 = JsoupUtils.selectFirst(SUBSECTION_I_2_SELECTOR, document);
        final Element subsectionI3 = JsoupUtils.selectFirst(SUBSECTION_I_3_SELECTOR, document);
        final Element subsectionI4 = JsoupUtils.selectFirst(SUBSECTION_I_4_SELECTOR, document);
        final Element subsectionII1 = JsoupUtils.selectFirst(SUBSECTION_II_1_SELECTOR, document);
        final Element subsectionIV1 = JsoupUtils.selectFirst(SUBSECTION_IV_1_SELECTOR, document);
        final Element subsectionIV3 = JsoupUtils.selectFirst(SUBSECTION_IV_3_SELECTOR, document);

        parsedTender
                .setPublications(parsePublications(document, subsectionIV3, url))
                .addBuyer(new ParsedBody()
                        .setName(JsoupUtils.selectText("a[name=KorisnikNaziv1] + span", subsectionI1))
                        .addBodyId(new BodyIdentifier()
                                .setId(JsoupUtils.selectText("a[name=KorisnikOIB1] + span", subsectionI1)))
                        .setAddress(new ParsedAddress()
                                .setStreet(JsoupUtils.selectText("a[name=KorisnikUlicaIBroj1] + span", subsectionI1))
                                .setCity(JsoupUtils.selectText("a[name=KorisnikGrad1] + span", subsectionI1))
                                .setPostcode(JsoupUtils.selectText("a[name=KorisnikPbr1] + span", subsectionI1))
                                .setCountry(JsoupUtils.selectText("a[name=KorisnikDrzava1] + span", subsectionI1))
                                .setUrl(JsoupUtils.selectText("a[name=KontGlavnaStranica1] + span", subsectionI1)))
                        .setContactName(JsoupUtils.selectText("a[name=ImeiPrezrezime1] + span", subsectionI1))
                        .setPhone(JsoupUtils.selectText("a[name=TelOZZP1] + span", subsectionI1))
                        .setEmail(JsoupUtils.selectText("a[name=EMailOZZP1] + span", subsectionI1))
                        .setBuyerType(JsoupUtils.selectText(CHECKBOX_TEXT_SELECTOR, subsectionI2))
                        .addMainActivity(JsoupUtils.selectText(CHECKBOX_TEXT_SELECTOR, subsectionI3)))
                .setIsOnBehalfOf(JsoupUtils.exists("input[name=NabPoNaloguDru_NE1][checked]", subsectionI4)
                        .toString())
                .setTitle(JsoupUtils.selectText(String.format(TEXT_AFTER_TITLE_SELECTOR_PATTERN, "II.1.1"),
                        subsectionII1))
                .setSupplyType(JsoupUtils.selectText(
                        "tr:has(input[name=VrstaPredmeta_RAD1]) > td > p:has(input[checked])", subsectionII1))
                .setAddressOfImplementation(new ParsedAddress()
                        .setRawAddress(JsoupUtils.selectText(String.format(TEXT_AFTER_TITLE_SELECTOR_PATTERN,
                                "Glavno mjesto ili lokacija radova, isporuke robe ili pružanja usluga:"),
                                subsectionII1))
                        .addNuts(JsoupUtils.selectText("a[name=NUTS1] + span", subsectionII1)))
                .setIsFrameworkAgreement(JsoupUtils.exists("input[name=Poziv_OS1][checked]", subsectionII1)
                        .toString())
                .addCpv(new ParsedCPV()
                        .setIsMain(Boolean.TRUE.toString())
                        .setCode(JsoupUtils.selectText("a[name=CpvOznaka1] + span", subsectionII1)))
                .setIsCoveredByGpa(EOJNTenderOldAndNewFormUtils.parseIsTenderCoveredByGpa(subsectionI4))
                .setProcedureType(JsoupUtils.selectText("td:has(input[checked]) + td", subsectionIV1))
                .setBuyerAssignedId(JsoupUtils.selectText(String.format(TEXT_AFTER_TITLE_SELECTOR_PATTERN, "IV.3.1"),
                        subsectionIV3))
                .addFunding(new ParsedFunding()
                        .setIsEuFund(EOJNTenderOldAndNewFormUtils.parseBooleanFromCheckboxes(
                                "NabavaPoPlanuEU_DA1", "NabavaPoPlanuEU_NE1", document)));

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
        PublicationFormType formType = getFormType(parsedTender.getPublications().get(0).getSourceFormType());
        switch (formType) {
            case CONTRACT_NOTICE:
                return EOJNTenderOldContractNoticeHandler.parse(parsedTender, document);
            case CONTRACT_AWARD:
                return EOJNTenderOldContractAwardHandler.parse(parsedTender, document);
            default:
                // no operation
                return parsedTender;
        }
    }

    /**
     * Gets type of form for given raw tender publication text.
     *
     * @param sourceFormType
     *         raw tender publication text
     *
     * @return type of form for provided raw tender publication text
     */
    private static PublicationFormType getFormType(final String sourceFormType) {
        switch (sourceFormType) {
            case "Poziv na nadmetanje":
                return PublicationFormType.CONTRACT_NOTICE;
            case "Obavijest o sklopljenim ugovorima":
                return PublicationFormType.CONTRACT_AWARD;
            default:
                return PublicationFormType.OTHER;
        }
    }

    /**
     * Parses info about this publication and all the related and previous ones.
     *
     * @param document
     *         parsed document tree for the source HTML page
     * @param subsectionIV3Element
     *         subsection IV.3 to be parsed
     * @param url
     *         source data URL
     *
     * @return list of all parsed publications including previous ones and other related ones
     */
    private static List<ParsedPublication> parsePublications(final Document document,
                                                             final Element subsectionIV3Element,
                                                             final String url) {
        final List<ParsedPublication> publications = new ArrayList<>();

        // this publication
        publications.add(new ParsedPublication()
                .setIsIncluded(true)
                .setHumanReadableUrl(url)
                .setSource(PublicationSources.HR_EOJN)
                .setSourceFormType(EOJNTenderOldAndNewFormUtils.parsePublicationSourceFormType(document))
                .setSourceId(EOJNTenderOldAndNewFormUtils.parsePublicationSourceId(document)));

        // previous publications
        publications.addAll(parsePreviousPublications(subsectionIV3Element));

        return publications;
    }

    /**
     * Parses information about previous publications (usually in TED) related to
     * the same tender. This information is typically provided in the IV.3.2
     * subsection.
     *
     * @param subsectionIV3
     *         subsection IV.3 to be parsed
     *
     * @return list of previous publications related to the same tender or empty
     * list if no such publications found
     */
    private static List<ParsedPublication> parsePreviousPublications(final Element subsectionIV3) {
        final List<ParsedPublication> publications = new ArrayList<>();

        // "RanijaObj_Da1" is in CA and "RanijaObjava_DA1" is in CFT
        final Boolean existPreviousPublications = JsoupUtils.exists(
                "input[name=RanijaObj_Da1][checked], input[name=RanijaObjava_DA1][checked]", subsectionIV3);
        if (!existPreviousPublications) {
            return publications;
        }

        // we get source form type from the whole row, because sometimes ut is in span and sometimes it is in td. See
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

}
