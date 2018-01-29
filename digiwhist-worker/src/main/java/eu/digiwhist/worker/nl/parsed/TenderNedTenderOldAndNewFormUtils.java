package eu.digiwhist.worker.nl.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
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
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Utilities for TenderNed old and new forms.
 *
 * @author Marek Mikes
 */
final class TenderNedTenderOldAndNewFormUtils {
    private static final Logger logger = LoggerFactory.getLogger(TenderNedTenderOldAndNewFormUtils.class);

    // selectors of subsection contents in old and new forms
    static final String OLD_AND_NEW_SUBSECTION_I_1_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS1'] ~ h4:containsOwn(I.1)";
    static final String OLD_AND_NEW_SUBSECTION_I_2_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS1'] ~ h4:containsOwn(I.2)";
    static final String OLD_AND_NEW_SUBSECTION_I_3_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS1'] ~ h4:containsOwn(I.3)";
    static final String OLD_AND_NEW_SUBSECTION_I_4_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS1'] ~ h4:containsOwn(I.4)";
    static final String OLD_AND_NEW_SUBSECTION_I_5_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS1'] ~ h4:containsOwn(I.5)";
    static final String OLD_AND_NEW_SUBSECTION_II_1_1_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS2'] ~ h5:has(span:containsOwn(II.1.1))";
    static final String OLD_AND_NEW_SUBSECTION_II_1_2_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS2'] ~ h5:has(span:containsOwn(II.1.2))";
    static final String OLD_AND_NEW_SUBSECTION_II_1_3_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS2'] ~ h5:has(span:containsOwn(II.1.3))";
    static final String OLD_AND_NEW_SUBSECTION_II_1_4_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS2'] ~ h5:has(span:containsOwn(II.1.4))";
    static final String OLD_AND_NEW_SUBSECTION_II_1_5_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS2'] ~ h5:has(span:containsOwn(II.1.5))";
    static final String OLD_AND_NEW_SUBSECTION_II_1_6_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS2'] ~ h5:has(span:containsOwn(II.1.6))";
    static final String OLD_AND_NEW_SUBSECTION_II_1_7_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS2'] ~ h5:has(span:containsOwn(II.1.7))";
    static final String OLD_AND_NEW_SUBSECTION_II_1_8_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS2'] ~ h5:has(span:containsOwn(II.1.8))";
    static final String OLD_AND_NEW_SUBSECTION_II_1_9_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS2'] ~ h5:has(span:containsOwn(II.1.9))";
    static final String OLD_AND_NEW_SUBSECTION_II_2_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS2'] ~ h4:containsOwn(II.2)";
    static final String OLD_AND_NEW_SUBSECTION_II_2_13_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS2'] ~ h5:has(span:containsOwn(II.2.13))";
    static final String OLD_AND_NEW_SUBSECTION_II_2_14_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS2'] ~ h5:has(span:containsOwn(II.2.14))";
    static final String OLD_AND_NEW_SUBSECTION_II_3_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS2'] ~ h4:containsOwn(II.3)";
    static final String OLD_AND_NEW_SUBSECTION_III_1_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS3'] ~ h4:containsOwn(III.1)";
    static final String OLD_AND_NEW_SUBSECTION_IV_1_1_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS4'] ~ h5:has(span:containsOwn(IV.1.1))";
    static final String OLD_AND_NEW_SUBSECTION_IV_1_2_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS4'] ~ h5:has(span:containsOwn(IV.1.2))";
    static final String OLD_AND_NEW_SUBSECTION_IV_1_3_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS4'] ~ h5:has(span:containsOwn(IV.1.3))";
    static final String OLD_AND_NEW_SUBSECTION_IV_1_4_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS4'] ~ h5:has(span:containsOwn(IV.1.4))";
    static final String OLD_AND_NEW_SUBSECTION_IV_2_1_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS4'] ~ h5:has(span:containsOwn(IV.2.1))";
    static final String OLD_AND_NEW_SUBSECTION_IV_2_2_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS4'] ~ h5:has(span:containsOwn(IV.2.2))";
    static final String OLD_AND_NEW_SUBSECTION_IV_2_3_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS4'] ~ h5:has(span:containsOwn(IV.2.3))";
    static final String OLD_AND_NEW_SUBSECTION_IV_3_2_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS4'] ~ h5:has(span:containsOwn(IV.3.2))";
    static final String OLD_AND_NEW_SUBSECTION_IV_3_3_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS4'] ~ h5:has(span:containsOwn(IV.3.3))";
    static final String OLD_AND_NEW_SUBSECTION_IV_3_4_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS4'] ~ h5:has(span:containsOwn(IV.3.4))";
    static final String OLD_AND_NEW_SUBSECTION_IV_3_5_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS4'] ~ h5:has(span:containsOwn(IV.3.5))";
    static final String OLD_AND_NEW_SECTION_V_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS5']";
    static final String OLD_AND_NEW_SECTION_V_FIRST_ELEMENT_OF_FIRST_LOT_SELECTOR =
            "h3[id='detail-publicatie:linkS5'] + h4";
    static final String OLD_AND_NEW_SUBSECTION_VI_1_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS6'] ~ h4:containsOwn(VI.1)";
    static final String OLD_AND_NEW_SUBSECTION_VI_2_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS6'] ~ h4:containsOwn(VI.2)";
    static final String OLD_AND_NEW_SUBSECTION_VI_3_TITLE_SELECTOR =
            "h3[id='detail-publicatie:linkS6'] ~ h4:containsOwn(VI.3)";
    static final String OLD_AND_NEW_SECTION_B_FIRST_ELEMENT_OF_FIRST_LOT_SELECTOR =
            "h3[id='detail-publicatie:linkA2'] + h4";

    /**
     * Private constructor to make this class static.
     */
    private TenderNedTenderOldAndNewFormUtils() {
    }

    /**
     * Parses common attributes of old and new contract notice and contract award.
     *
     * @param parsedTender tender to add data to
     * @param form         document to be parsed
     */
    static void parseCommonAttributes(final ParsedTender parsedTender, final Element form) {
        parsedTender
            .addBuyer(new ParsedBody()
                .setName(parseBuyerName(form))
                .addBodyId(TenderNedTenderFormUtils.parseBodyIdentifier(parseBuyerBodyIdentifierId(form)))
                .setAddress(new ParsedAddress()
                    .setStreet(parseBuyerStreet(form))
                    .setCity(parseBuyerCity(form))
                    .addNuts(parseBuyerNuts(form))
                    .setPostcode(parseBuyerPostcode(form))
                    .setCountry(parseBuyerCountry(form)))
                .setContactName(parseBuyerContactName(form))
                .setEmail(parseBuyerEmail(form)))
            .setTitle(parseTenderTitle(form))
            .setNationalProcedureType(TenderNedTenderOldAndNewFormUtils.parseTenderNationalProcedureType(form));

            StringBuilder description = new StringBuilder();
            Element descriptionNode = JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_4_TITLE_SELECTOR, form);
            if (descriptionNode != null) {
                descriptionNode = descriptionNode.nextElementSibling();
                while (descriptionNode.tagName().equals("p")) {
                    description.append(descriptionNode.text()).append("\n");
                    descriptionNode = descriptionNode.nextElementSibling();
                }
                parsedTender.setDescription(description.length() == 0 ? null : description.toString());
            }
    }

    /**
     * Parse buyer URL from old document.
     *
     * @param form document to be parsed
     * @return String or Null
     */
    static String parseOldBuyerUrl(final Element form) {
        String urlRow = getFromOldElement(getSubsectionI1(form), "p:containsOwn(Internetadres\\(sen\\):)",
                "Adres van de aanbestedende dienst:");
        return getUrlFromUrlRow(urlRow);
    }

    /**
     * Parse buyer URL from new document.
     *
     * @param form document to be parsed
     * @return String or Null
     */
    static String parseNewBuyerUrl(final Element form) {
        String urlRow = getFromOldElement(getSubsectionI1(form), "h5:contains(Internetadres\\(sen\\)) + p",
                "Hoofdadres:");
        return getUrlFromUrlRow(urlRow);
    }

    /**
     * Parse documents URL from old document.
     *
     * @param form document to be parsed
     * @return String or Null
     */
    static String parseOldTenderDocumentsUrl(final Element form) {
        String urlRow = getFromOldElement(getSubsectionI1(form), "p:containsOwn(Internetadres\\(sen\\):)",
                "Elektronische toegang tot informatie:");
        return getUrlFromUrlRow(urlRow);
    }

    /**
     * Parses address of implementation from old document.
     *
     * @param form document to be parsed
     * @return address of implementation or null
     */
    static ParsedAddress parseOldTenderAddressOfImplementation(final Element form) {
        final Element subsection = ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_2_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_3_TITLE_SELECTOR, form));

        final String rawAddressTitle1 = "Belangrijkste plaats van dienstverlening:";
        String rawAddress = getFromOldElement(subsection, "p:containsOwn(" + rawAddressTitle1 + ")", rawAddressTitle1);
        if (rawAddress == null) {
            final String rawAddressTitle2 = "Belangrijkste plaats van uitvoering van de werken:";
            rawAddress = getFromOldElement(subsection, "p:containsOwn(" + rawAddressTitle2 + ")",
                    rawAddressTitle2);
        }

        if (rawAddress == null) {
            return null;
        }

        final ParsedAddress result = new ParsedAddress()
                .setRawAddress(rawAddress);

        Elements nutsElements = JsoupUtils.select("dl > dd", subsection);
        if (!nutsElements.isEmpty()) {
            result
                    .setNuts(nutsElements
                            .stream()
                            .map(n -> n.text())
                            .collect(Collectors.toList()));
        }

        return result;
    }

    /**
     * Parse if there is framework agreement from old document.
     *
     * @param form document to be parsed
     * @return string or null
     */
    static String parseOldTenderIsFrameworkAgreement(final Element form) {
        final Element subsection = ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_3_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_4_TITLE_SELECTOR, form));

        assert JsoupUtils.select("ul > li", subsection).size() <= 1;

        final String isFrameworkAgreementString = JsoupUtils.selectText("ul > li", subsection);

        if (isFrameworkAgreementString == null) {
            return null;
        }

        switch (isFrameworkAgreementString) {
            // public contract
            case "De aankondiging betreft een overheidsopdracht":
            // dynamic purchasing system
            case "De aankondiging betreft de instelling van een dynamisch aankoopsysteem (DAS)":
            case "De aankondiging betreft een opdracht (opdrachten) gebaseerd op een dynamisch aankoopsysteem (DAS)":
            case "-":
                return Boolean.FALSE.toString();
            case "De aankondiging betreft de opstelling van een raamovereenkomst":
                return Boolean.TRUE.toString();
            default:
                logger.error("We have new isFrameworkAgreement value \"{}\"", isFrameworkAgreementString);
                return null;
        }
    }

    /**
     * Parse if there is framework agreement from new document.
     *
     * @param form document to be parsed
     * @return string or null
     */
    static String parseNewTenderIsFrameworkAgreement(final Element form) {
        final Element subsection = ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_IV_1_3_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_IV_1_4_TITLE_SELECTOR, form));

        final String isFrameworkAgreementString = JsoupUtils.selectText("p", subsection);

        if (isFrameworkAgreementString == null) {
            return null;
        }

        switch (isFrameworkAgreementString) {
            // dynamic purchasing system
            case "De aanbesteding houdt de instelling van een dynamisch aankoopsysteem in":
            case "Er is een dynamisch aankoopsysteem ingesteld":
            case "-":
                return Boolean.FALSE.toString();
            case "De aanbesteding betreft het sluiten van een raamovereenkomst":
                return Boolean.TRUE.toString();
            default:
                logger.error("We have new isFrameworkAgreement value \"{}\"", isFrameworkAgreementString);
                return null;
        }
    }

    /**
     * Parse tender CPVs from old document.
     *
     * @param subsection
     *         subsection to be parsed
     *
     * @return parsed CPVs or null
     */
    static List<ParsedCPV> parseOldTenderCpvs(final Element subsection) {
        Element mainTaskElement = JsoupUtils.selectFirst("dl", subsection);
        if (mainTaskElement == null) {
            return null;
        }

        List<ParsedCPV> cpvs = new ArrayList<>();

        String cpv = ParserUtils.getFromContent(mainTaskElement, "dt:containsOwn(Hoofdcategorieën:) + dd", 0);
        if (cpv != null) {
            cpvs.add(new ParsedCPV()
                    .setIsMain(Boolean.TRUE.toString())
                    .setCode(cpv.contains("-") ? cpv.substring(0, cpv.indexOf('-')).trim() : cpv));
        }

        assert JsoupUtils.select("dt:containsOwn(Subcategorieën:) + dd", mainTaskElement).size() <= 1;
        cpv = ParserUtils.getFromContent(mainTaskElement, "dt:containsOwn(Subcategorieën:) + dd", 0);
        if (cpv != null) {
            cpvs.add(new ParsedCPV()
                    .setIsMain(Boolean.FALSE.toString())
                    .setCode(cpv.contains("-") ? cpv.substring(0, cpv.indexOf('-')).trim() : cpv));
        }

        return cpvs;
    }

    /**
     * Parse tender CPVs from new document.
     *
     * @param form
     *         form to be parsed
     *
     * @return parsed CPVs or null
     */
    static List<ParsedCPV> parseNewTenderCpvs(final Element form) {
        Element mainTaskElement = JsoupUtils.selectFirst("dl", ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_2_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_3_TITLE_SELECTOR, form)));
        if (mainTaskElement == null) {
            return null;
        }

        List<ParsedCPV> cpvs = new ArrayList<>();

        String cpv = ParserUtils.getFromContent(mainTaskElement, "dt:containsOwn(Hoofdcategorie:) + dd", 0);
        if (cpv != null) {
            cpvs.add(new ParsedCPV()
                    .setIsMain(Boolean.TRUE.toString())
                    .setCode(cpv.contains("-") ? cpv.substring(0, cpv.indexOf('-')).trim() : cpv));
        }

        assert JsoupUtils.select("dt:containsOwn(Subcategorie:) + dd", mainTaskElement).size() <= 1;
        cpv = ParserUtils.getFromContent(mainTaskElement, "dt:containsOwn(Subcategorie:) + dd", 0);
        if (cpv != null) {
            cpvs.add(new ParsedCPV()
                    .setIsMain(Boolean.FALSE.toString())
                    .setCode(cpv.contains("-") ? cpv.substring(0, cpv.indexOf('-')).trim() : cpv));
        }

        return cpvs;
    }

    /**
     * Parse if tender has lots value from document.
     * Probably the information is in old CFT and new CFT and CA.
     *
     * @param subsection
     *         subsection to be parsed
     *
     * @return String or null
     */
    static String parseIfTenderHasLots(final Element subsection) {
        String ifTenderHasLotsText = JsoupUtils.selectText("p", subsection);
        if (ifTenderHasLotsText == null) {
            return null;
        }

        final String title = "Verdeling in percelen:";
        assert ifTenderHasLotsText.startsWith(title);
        ifTenderHasLotsText = ifTenderHasLotsText.substring(title.length()).trim();
        // Text after title can be:
        //  - "neen"
        //  - "ja Inschrijvingen mogen worden ingediend voor: alle percelen"
        //  - "ja Inschrijvingen mogen worden ingediend voor: één perceel"
        //  - "ja Inschrijvingen mogen worden ingediend voor: een of meer percelen"
        return BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(ifTenderHasLotsText.split(" ")[0]));
    }

    /**
     * Parse tender bid deadline value from subsection.
     *
     * @param subsection
     *         subsection to be parsed
     *
     * @return String or Null
     */
    static String parseTenderBidDeadline(final Element subsection) {
        return ParserUtils.getFromContent(subsection, "p", "Datum:");
    }

    /**
     * Parse tender funding from subsection.
     *
     * @param subsection
     *         subsection to be parsed
     *
     * @return ParsedFunding or Null
     */
    static ParsedFunding parseTenderFunding(final Element subsection) {
        String isEuFundString = ParserUtils.getFromContent(subsection, "p",
                "De opdracht houdt verband met een project en/of een programma dat met middelen van de Europese Unie " +
                        "wordt gefinancierd:");
        if (isEuFundString == null) {
            isEuFundString = ParserUtils.getFromContent(subsection, "p",
                    "De aanbesteding houdt verband met een project en/of een programma dat met middelen van de EU " +
                            "wordt gefinancierd:");
        }

        if (isEuFundString == null) {
            return null;
        }

        final Boolean isEuFund = TenderNedTenderFormUtils.meansYes(isEuFundString);

        return isEuFund == null
                ? null
                : new ParsedFunding()
                        .setIsEuFund(isEuFund.toString());
    }

    /**
     * Parse buyer type value from old document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseOldTenderBuyerType(final Element form) {
        final Element subsection = ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_I_2_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_I_3_TITLE_SELECTOR, form));
        return ParserUtils.getFromContent(subsection, "p", 0);
    }

    /**
     * Parse buyer type value from new document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseNewTenderBuyerType(final Element form) {
        final Element subsection = ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_I_4_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_I_5_TITLE_SELECTOR, form));
        return ParserUtils.getFromContent(subsection, "p", 0);
    }

    /**
     * Parse previous publication in TED from new document.
     *
     * @param form
     *         document to be parsed
     *
     * @return previous publication in TED or Null
     */
    static ParsedPublication parseNewTenderPreviousPublicationInTed(final Element form) {
        final Element subsection = ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_IV_2_1_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_IV_2_2_TITLE_SELECTOR, form));
        final String tedId = ParserUtils.getFromContent(subsection, "p",
                "Nummer van de aankondiging in het PB S:");
        return tedId == null
                ? null
                : new ParsedPublication()
                .setIsIncluded(false)
                .setSource(PublicationSources.EU_TED)
                .setSourceId(tedId);
    }

    /**
     * Parse previous publication in TED from old document.
     *
     * @param form
     *         document to be parsed
     *
     * @return previous publication in TED or Null
     */
    static List<ParsedPublication> parseOldTenderPreviousPublicationsInTed(final Element form) {
        Element endElement = JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_IV_3_3_TITLE_SELECTOR, form);
        if (endElement == null) {
            endElement = JsoupUtils.selectFirst(OLD_AND_NEW_SECTION_V_TITLE_SELECTOR, form);
        }
        assert endElement != null;

        final Element subsection = ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_IV_3_2_TITLE_SELECTOR, form), endElement);
        final String arePreviousPublicationsInTedString = ParserUtils.getFromContent(subsection, "p", 0);
        if (arePreviousPublicationsInTedString == null) {
            return null;
        }

        final Boolean arePreviousPublicationsInTed = TenderNedTenderFormUtils.meansYes(
                arePreviousPublicationsInTedString);
        if (arePreviousPublicationsInTed == null) {
            logger.error("Unknown text \"{}\" which should mean yes/no in Dutch", arePreviousPublicationsInTedString);
            return null;
        } else if (!arePreviousPublicationsInTed) {
            return null;
        }

        // previous publications in TED are filled

        Elements children = subsection.children();
        if (children.size() < 4) {
            logger.error("There should be previous publications in TED, but they probably are not");
            return null;
        }
        children.remove(0); // remove subsection title (h5 element)
        children.remove(0); // remove information whether previous publications in TED exist (p element)
        children.remove(children.size() - 1); // remove footer element
        List<ParsedPublication> previousPublicationsInTed = new ArrayList<>();
        final String textBeforeSourceId = "Nummer van de aankondiging in het PBEU:";
        for (Element child : children) {
            String[] rows = child.html().split("<br>");
            if (rows.length != 2) {
                logger.error("Information about previous publications in TED should have two rows, but it has {} rows",
                        rows.length);
                return null;
            }
            assert rows[1].startsWith(textBeforeSourceId);
            final String[] sourceIdAndPublicationDate = rows[1].substring(textBeforeSourceId.length()).split("van");
            assert sourceIdAndPublicationDate.length == 2;
            final String sourceId = sourceIdAndPublicationDate[0].trim();
            final String publicationDate = sourceIdAndPublicationDate[1].trim();
            previousPublicationsInTed.add(new ParsedPublication()
                    .setIsIncluded(false)
                    .setSource(PublicationSources.EU_TED)
                    .setSourceFormType(rows[0])
                    .setSourceId(sourceId.equals("-") ? null : sourceId)
                    .setPublicationDate(publicationDate.equals("-") ? null : publicationDate));
        }
        return previousPublicationsInTed;
    }

    /**
     * Parse tender lots from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return list of lots or Null
     */
    static List<ParsedTenderLot> parseNewTenderLots(final Element form) {
        final String sectionFooterClassName = "section-footer";

        Element lotElement = JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_2_TITLE_SELECTOR, form);
        if (lotElement == null) {
            return null;
        }

        List<ParsedTenderLot> lots = new ArrayList<>();
        do {
            ParsedTenderLot lot = new ParsedTenderLot();

            lotElement = lotElement.nextElementSibling();

            // subsection 1
            if (lotElement.text().startsWith("II.2.1")) {
                lotElement = lotElement.nextElementSibling();
                String lotTitle = lotElement.ownText().trim();
                if (!lotTitle.isEmpty() && !lotTitle.equals("-")) {
                    lot.setTitle(lotTitle);
                }
                lotElement = lotElement.nextElementSibling();
                String lotNumber = lotElement.ownText().substring(lotElement.ownText().indexOf(':') + 1).trim();
                if (!lotNumber.isEmpty() && !lotNumber.equals("-")) {
                    lot.setLotNumber(lotNumber);
                }
                lotElement = goNextLotSubsection(lotElement);
            }
            // subsection 2
            if (lotElement.text().startsWith("II.2.2")) {
                lotElement = lotElement.nextElementSibling(); // the element is "dl" element
                String cpv = JsoupUtils.selectText("dl > dt:containsOwn(Hoofdcategorie:) + dd", lotElement);
                if (cpv != null) {
                    lot.addCpv(new ParsedCPV()
                        .setIsMain(Boolean.TRUE.toString())
                        .setCode(cpv.contains("-") ? cpv.substring(0, cpv.indexOf('-')).trim() : cpv));
                }

                // todo: uncomment code below when we get answer in Trello, because do not know how to parse
                // https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
                // 297d82335006a1fc724023b7520cc10e/pageId/D909A/huidigemenu/aankondigingen/cid/301506/cvp/join
                // assert JsoupUtils.select("dl > dt:containsOwn(Subcategorie:) + dd", lotElement).size() <= 1;
                cpv = ParserUtils.getFromContent(lotElement, "dl > dt:containsOwn(Subcategorie:) + dd", 0);
                if (cpv != null) {
                    lot.addCpv(new ParsedCPV()
                        .setIsMain(Boolean.FALSE.toString())
                        .setCode(cpv.contains("-") ? cpv.substring(0, cpv.indexOf('-')).trim() : cpv));
                }
                lotElement = goNextLotSubsection(lotElement);
            }
            lotElement = goThroughLotSubsection("II.2.3", lotElement);
            lotElement = goThroughLotSubsection("II.2.4", lotElement);

            // subsection II.2.5
            if (lotElement.text().startsWith("II.2.5")) {
                lotElement = lotElement.nextElementSibling();

                while (lotElement.tagName().equals("p")) {
                    Matcher m = Pattern.compile("(?im)^((Naam: (?<name>.+))|(?<price>Prijs))$(.*\n)*?"
                        + "^Weging: (?<weight>.+)$").matcher(lotElement.html().replace("<br>", "\n"));

                    if (m.find()) {
                        lot.addAwardCriterion(new ParsedAwardCriterion()
                            .setName(m.group("name") != null ? m.group("name") : m.group("price"))
                            .setWeight(m.group("weight")));
                    }

                    lotElement = goNextLotSubsection(lotElement);
                }
            }

            // subsection 6
            if (lotElement.text().startsWith("II.2.6")) {
                // this subsection is filled only in new contract notice

                lotElement = lotElement.nextElementSibling();
                lot.setEstimatedPrice(parseNewContractNoticePrice(lotElement.ownText()));
                lotElement = goNextLotSubsection(lotElement);
            }

            // subsection II.2.7
            if (lotElement.text().startsWith("II.2.7")) {
                lotElement = lotElement.nextElementSibling();

                Matcher m = Pattern.compile("(?im)^(Aanvang: (?<start>.*))|(Einde: (?<end>.*))$")
                    .matcher(lotElement.html().replace("<br>", "\n"));

                while (m.find()) {
                    if (m.group("start") != null) {
                        lot.setEstimatedStartDate(m.group("start"));
                    } else if (m.group("end") != null) {
                        lot.setEstimatedCompletionDate(m.group("end"));
                    }
                }

                lotElement = goNextLotSubsection(lotElement);
            }
            
            lotElement = goThroughLotSubsection("II.2.8", lotElement);
            lotElement = goThroughLotSubsection("II.2.9", lotElement);
            lotElement = goThroughLotSubsection("II.2.10", lotElement);
            
            // subsection 12
            if (lotElement.text().startsWith("II.2.11")) {
                lotElement = lotElement.nextElementSibling();
                
                lot.setHasOptions(BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils
                    .meansYes(lotElement.text().replace("Opties:", ""))));
                
                lotElement = goNextLotSubsection(lotElement);
            }

            lotElement = goThroughLotSubsection("II.2.12", lotElement);
            // subsection 13
            if (lotElement.text().startsWith("II.2.13")) {
                lotElement = lotElement.nextElementSibling();
                final String isEuFund = ParserUtils.getFromContent(lotElement, "p",
                        "De aanbesteding houdt verband met een project en/of een programma dat met middelen van de " +
                                "EU wordt gefinancierd:");
                if (isEuFund != null) {
                    lot.addFunding(new ParsedFunding()
                            .setIsEuFund(BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(isEuFund))));
                }
                lotElement = goNextLotSubsection(lotElement);
            }
            lotElement = goThroughLotSubsection("II.2.14", lotElement);

            lots.add(lot);

            // move to the first element of next lot or to the end element of lots
            while (lotElement != null && !lotElement.nodeName().equals("h4")
                && !lotElement.className().equals(sectionFooterClassName)) {
                lotElement = lotElement.nextElementSibling();
            }

            if (lotElement == null || lotElement.className().equals(sectionFooterClassName)) {
                break;
            }
        } while (true);
        return lots;
    }

    /**
     * Parse price value from string.
     *
     * @param priceString
     *         text to be parsed
     *
     * @return parsed price or Null
     */
    static ParsedPrice parseNewContractNoticePrice(final String priceString) {
        if (priceString == null) {
            return null;
        }

        ParsedPrice price = null;

        // Examples of price are:
        // - "Waarde zonder btw: 1 420 000,00 Munt: EUR"
        // - "Waarde zonder btw: 0,01 Munt: EUR"
        // - "Waarde zonder btw: - Munt: -"

        final String valueExcludingVat = "Waarde zonder btw:";
        final String currencyString = "Munt:";
        assert priceString.startsWith(valueExcludingVat) && priceString.contains(currencyString);
        final int currencyIndex = priceString.indexOf(currencyString);

        final String netAmount = priceString.substring(valueExcludingVat.length(), currencyIndex).trim();
        if (!netAmount.equals("-")) {
            price = new ParsedPrice().setNetAmount(netAmount);
        }

        final String currency = priceString.substring(currencyIndex + currencyString.length()).trim();
        if (!currency.equals("-")) {
            if (price == null) {
                price = new ParsedPrice();
            }
            price.setCurrency(currency);
        }

        return price;
    }

    /**
     * The method is convenience method for lot parsing. It shifts element pointer from the begin of one subsection to:
     *  - begin of next subsection when the next subsection exists. Otherwise to
     *  - begin of next lot section when the next lot section exists. Otherwise to
     *  - just end of lot information
     *
     * @param subsectionTitle
     *         title of subsection begin
     * @param element
     *         element which probably contains subsectionTitle. The element does not contain subsectionTitle when the
     *         element does not exist
     *
     * @return next (sub)section element after element with subsectionTitle
     */
    private static Element goThroughLotSubsection(final String subsectionTitle, final Element element) {
        Element newSubsectionElement = element;

        if (newSubsectionElement.text().startsWith(subsectionTitle)) {
            newSubsectionElement = goNextLotSubsection(newSubsectionElement.nextElementSibling());
        }

        return newSubsectionElement;
    }
    
    /**
     * The method is convenience method for lot parsing. It shifts element pointer from the begin of one subsection to:
     *  - begin of next subsection when the next subsection exists. Otherwise to
     *  - begin of next lot section when the next lot section exists. Otherwise to
     *  - just end of lot information
     *
     * @param element
     *         element which probably contains subsectionTitle. The element does not contain subsectionTitle when the
     *         element does not exist
     *
     * @return next (sub)section element after element with subsectionTitle
     */
    private static Element goNextLotSubsection(final Element element) {
        Element nextSubsection = element;

        final String sectionFooterClassName = "section-footer";
        
        while (nextSubsection != null && !nextSubsection.nodeName().equals("h5") // next subsection
                    && !nextSubsection.nodeName().equals("h4") // next lot
                    && !nextSubsection.className().equals(sectionFooterClassName)) {
            nextSubsection = nextSubsection.nextElementSibling();
        }
        return nextSubsection;
    }



    /**
     * Gets URL from row containing URL.
     *
     * @param urlRow
     *         row containing URL
     *
     * @return String or Null
     */
    private static String getUrlFromUrlRow(final String urlRow) {
        if (urlRow == null) {
            return null;
        } else if (urlRow.contains("\"")) {
            // the string on the web row is something like
            // <a href="http://www.heerhugowaard.nl">www.heerhugowaard.nl</a>"
            assert urlRow.contains("\">");
            return urlRow.substring(urlRow.indexOf('"') + 1, urlRow.indexOf("\">"));
        } else {
            // the string on row is something like "www.montferland.info"
            return urlRow;
        }
    }

    /**
     * Gets subsection I.1 wrapped into one element.
     *
     * @param form
     *         document to be parsed
     *
     * @return subsection I.1 wrapped into one element or Null
     */
    private static Element getSubsectionI1(final Element form) {
        return ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_I_1_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_I_2_TITLE_SELECTOR, form));
    }

    /**
     * Parse buyer name from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseBuyerName(final Element form) {
        return JsoupUtils.selectText("dl > dt:containsOwn(Officiële benaming:) + dd", getSubsectionI1(form));
    }

    /**
     * Parse tender national procedure type value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderNationalProcedureType(final Element form) {
        final Element subsection = ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_IV_1_1_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_IV_1_2_TITLE_SELECTOR, form));
        return JsoupUtils.selectText("p", subsection);
    }

    /**
     * Parse buyer body identifier ID from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseBuyerBodyIdentifierId(final Element form) {
        return JsoupUtils.selectText("dl > dt:containsOwn(Nationale identificatie:) + dd", getSubsectionI1(form));
    }

    /**
     * Parse buyer street from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseBuyerStreet(final Element form) {
        return JsoupUtils.selectText("dl > dt:containsOwn(Postadres:) + dd", getSubsectionI1(form));
    }

    /**
     * Parse buyer city from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseBuyerCity(final Element form) {
        return JsoupUtils.selectText("dl > dt:containsOwn(Plaats:) + dd", getSubsectionI1(form));
    }

    /**
     * Parse buyer NUTS code from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseBuyerNuts(final Element form) {
        return JsoupUtils.selectText("dl > dt:containsOwn(NUTS-code:) + dd", getSubsectionI1(form));
    }

    /**
     * Parse buyer postcode from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseBuyerPostcode(final Element form) {
        return JsoupUtils.selectText("dl > dt:containsOwn(Postcode:) + dd", getSubsectionI1(form));
    }

    /**
     * Parse buyer country from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseBuyerCountry(final Element form) {
        return JsoupUtils.selectText("dl > dt:containsOwn(Land:) + dd", getSubsectionI1(form));
    }

    /**
     * Parse buyer contact name from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseBuyerContactName(final Element form) {
        return JsoupUtils.selectText(
                "dl > dt:containsOwn(Contactpersoon:) + dd, dl > dt:containsOwn(Ter attentie van:) + dd",
                getSubsectionI1(form));
    }

    /**
     * Parse buyer email from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseBuyerEmail(final Element form) {
        return JsoupUtils.selectText("dl > dt:containsOwn(E-mail:) + dd", getSubsectionI1(form));
    }

    /**
     * Parse tender title from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderTitle(final Element form) {
        final Element subsection = ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_1_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_2_TITLE_SELECTOR, form));
        return JsoupUtils.selectText("p", subsection);
    }

    /**
     * The method gets desired row text which is after title row from some element in an old format form.
     *
     * @param element
     *         element to parse data from
     * @param selector
     *         selector which selects element containing many rows. It can select more than one element, but
     *         we want the first one
     * @param informationTitle
     *         title (text on some some row below which is desired row)
     *
     * @return row from some element in an old format form or null if the row is not filled (row is "-")
     */
    private static String getFromOldElement(final Element element, final String selector,
                                            final String informationTitle) {
        Element foundElement = JsoupUtils.selectFirst(selector, element);
        if (foundElement == null) {
            return null;
        }
        String[] rows = foundElement.html().split("<br>");
        for (int i = 0; i < rows.length - 1; ++i) {
            if (rows[i].equals(informationTitle)) {
                String result = rows[i + 1].trim();
                return result.equals("-") ? null : result;
            }
        }

        return null;
    }


    /**
     * Parse if tender is covered by GPA.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    public static String parseIsTenderCoveredByGpa(final Element form) {
        String gpa = ParserUtils.getFromContent(form, "h5:has(span:containsOwn(II.1.6)) + p",
            "Opdracht valt onder de Overeenkomst inzake overheidsopdrachten (GPA):");

        if (gpa == null) {
            gpa = ParserUtils.getFromContent(form, "h5:has(span:containsOwn(IV.1.8)) + p",
                "De opdracht valt onder de GPA:");
        }

        return BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(gpa));
    }


    /**
     * Parse supply type of tender from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseTenderSupplyType(final Element form) {
        String supplyType = JsoupUtils.selectText("h2.type-contract", form);
        if (supplyType == null) {
            supplyType = ParserUtils.getFromContent(form, OLD_AND_NEW_SUBSECTION_II_1_2_TITLE_SELECTOR + " + p",
                "Type:");
        }

        return supplyType;
    }


    /**
     * Parse buyer assigned id of tender from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    static String parseBuyerAssignedId(final Element form) {
        return ParserUtils.getFromContent(form, OLD_AND_NEW_SUBSECTION_II_1_1_TITLE_SELECTOR
            + " ~ p:containsOwn(Referentienummer:)", "Referentienummer:");
    }
}
