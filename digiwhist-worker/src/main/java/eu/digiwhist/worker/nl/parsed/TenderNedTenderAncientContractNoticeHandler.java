package eu.digiwhist.worker.nl.parsed;

import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils
        .ANCIENT_SECTION_B_FIRST_ELEMENT_OF_FIRST_LOT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_III_1_1_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_III_2_1_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_III_2_2_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_III_2_3_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_II_1_2_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_II_1_5_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_II_1_6_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_II_1_7_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_II_1_9_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_II_2_2_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_II_3_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_IV_3_4_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_IV_3_6_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_VI_1_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_VI_2_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils
        .ANCIENT_SUBSECTION_VI_4_1_1_CONTENT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils
        .ANCIENT_SUBSECTION_VI_4_1_2_CONTENT_SELECTOR;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Element;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parser for TenderNed ancient contract notice form specific data.
 *
 * @author Marek Mikes
 */
final class TenderNedTenderAncientContractNoticeHandler {
    /**
     * Private constructor to make this class static.
     */
    private TenderNedTenderAncientContractNoticeHandler() {
    }

    /**
     * Parse method for TenderNed ancient contract notice form from specific data.
     *
     * @param parsedTender
     *         tender to add data to
     * @param form
     *         document to parse data from
     *
     * @return ParsedTender with data added
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Element form) {
        return parsedTender
            .addBuyer(TenderNedTenderAncientFormUtils.parseFirstTenderBuyer(form))
            .addBuyer(TenderNedTenderAncientFormUtils.parseSecondTenderBuyer(form))
            .setSupplyType(TenderNedTenderAncientFormUtils.parseTenderSupplyType(form))
            .setOnBehalfOf(TenderNedTenderAncientFormUtils.parseTenderOnBehalfOf(form))
            .setTitle(TenderNedTenderAncientFormUtils.parseTenderTitle(form))
            .setAddressOfImplementation(new ParsedAddress()
                .setRawAddress(parseRawAddressOfImplementation(form))
                .addNuts(TenderNedTenderAncientFormUtils.parseAddressOfImplementationNuts(form)))
            .setIsFrameworkAgreement(TenderNedTenderAncientFormUtils.parseIsTenderFrameworkAgreement(form))
            .setDescription(TenderNedTenderAncientFormUtils.parseTenderDescription(form,
                ANCIENT_SUBSECTION_II_1_5_CONTENT_SELECTOR))
            .setCpvs(TenderNedTenderAncientFormUtils.parseTenderCpvs(form, ANCIENT_SUBSECTION_II_1_6_CONTENT_SELECTOR))
            .setIsCoveredByGpa(parseIsTenderCoveredByGpa(form))
            .setAreVariantsAccepted(parseAreVariantsAccepted(form))
            .setHasOptions(parseIfTenderHasOptions(form))
            .setEstimatedDurationInMonths(parseTenderEstimatedDurationInMonths(form))
            .setEstimatedStartDate(parseTenderEstimatedStartDate(form))
            .setEstimatedCompletionDate(parseTenderEstimatedCompletionDate(form))
            .setDeposits(parseTenderDeposits(form))
            .setPersonalRequirements(parseTenderPersonalRequirements(form))
            .setEconomicRequirements(parseTenderEconomicRequirements(form))
            .setTechnicalRequirements(parseTenderTechnicalRequirements(form))
            .setNationalProcedureType(TenderNedTenderAncientFormUtils.parseTenderNationalProcedureType(form))
            .setAwardCriteria(TenderNedTenderAncientFormUtils.parseAwardCriteria(form))
            .setIsElectronicAuction(TenderNedTenderAncientFormUtils.parseIfTenderIsElectronicAuction(form))
            .setBuyerAssignedId(TenderNedTenderAncientFormUtils.parseTenderBuyerAssignedId(form))
            .setEligibleBidLanguages(parseTenderEligibleBidLanguages(form))
            .setIsDps(parseIfTenderIsDps(form))
            .addFunding(new ParsedFunding().setIsEuFund(parseIsEuFunded(form)))
            .setAppealBodyName(parseTenderAppealBodyName(form))
            .setMediationBodyName(parseTenderMediationBodyName(form))
            .setLots(parseTenderLots(form))
            .setSelectionMethod(TenderNedTenderAncientFormUtils.parseSelectionMethod(form))
            .setHasLots(BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(
                ParserUtils.getFromContent(form, "h4:has(span:containsOwn(II.1.8)) + p", 0))))
            .setBidDeadline(parseTenderBidDeadline(form));
    }

    /**
     * Parse raw address of implementation from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseRawAddressOfImplementation(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_II_1_2_CONTENT_SELECTOR,
                "Belangrijkste plaats van uitvoering van de werken:");
    }

    /**
     * Parse if tender is covered by GPA.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseIsTenderCoveredByGpa(final Element form) {
        return BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(
                ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_II_1_7_CONTENT_SELECTOR, 0)));
    }

    /**
     * Parse if variants are accepted value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseAreVariantsAccepted(final Element form) {
        return BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(
                ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_II_1_9_CONTENT_SELECTOR, 0)));
    }

    /**
     * Parse if tender has options value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseIfTenderHasOptions(final Element form) {
        return BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(
                ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_II_2_2_CONTENT_SELECTOR, 0)));
    }

    /**
     * Parse tender estimated duration in months value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderEstimatedDurationInMonths(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_II_3_CONTENT_SELECTOR,
                "Periode in maanden:");
    }

    /**
     * Parse tender estimated start date value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderEstimatedStartDate(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_II_3_CONTENT_SELECTOR, "Aanvang:");
    }

    /**
     * Parse tender estimated completion date value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderEstimatedCompletionDate(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_II_3_CONTENT_SELECTOR, "Voltooiing:");
    }

    /**
     * Parse tender deposits value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderDeposits(final Element form) {
        // return the whole subsection content (annotation does not say something more specific)
        return JsoupUtils.selectText(ANCIENT_SUBSECTION_III_1_1_CONTENT_SELECTOR, form);
    }

    /**
     * Parse tender personal requirements value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderPersonalRequirements(final Element form) {
        // return the whole subsection content (annotation does not say something more specific)
        return JsoupUtils.selectText(ANCIENT_SUBSECTION_III_2_1_CONTENT_SELECTOR, form);
    }

    /**
     * Parse tender technical requirements value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderTechnicalRequirements(final Element form) {
        // return the whole subsection content (annotation does not say something more specific)
        return JsoupUtils.selectText(ANCIENT_SUBSECTION_III_2_3_CONTENT_SELECTOR, form);
    }

    /**
     * Parse tender economic requirements value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderEconomicRequirements(final Element form) {
        // return the whole subsection content (annotation does not say something more specific)
        return JsoupUtils.selectText(ANCIENT_SUBSECTION_III_2_2_CONTENT_SELECTOR, form);
    }

    /**
     * Parse tender bid deadline value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderBidDeadline(final Element form) {
        String deadline = ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_IV_3_4_CONTENT_SELECTOR, 0);
        if (deadline == null) {
            return null;
        }

        // the deadline has format "20/12/2012 11:00" or "20/12/2012"
        return deadline.split(" ")[0];
    }

    /**
     * Parse tender eligible bid languages from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return list of languages or Null
     */
    private static List<String> parseTenderEligibleBidLanguages(final Element form) {
        List<String> languages = new ArrayList<>();
        languages.add(ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_IV_3_6_CONTENT_SELECTOR, 0));
        String otherLanguages = ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_IV_3_6_CONTENT_SELECTOR, "Andere:");
        if (otherLanguages != null) {
            languages.add(otherLanguages);
        }
        return languages;
    }

    /**
     * Parse if tender is dynamic purchasing system value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseIfTenderIsDps(final Element form) {
        return BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(
                ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_VI_1_CONTENT_SELECTOR, 0)));
    }

    /**
     * Parse if source is from EU funds from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseIsEuFunded(final Element form) {
        return BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(
                ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_VI_2_CONTENT_SELECTOR, 0)));
    }

    /**
     * Parse tender appeal body name value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderAppealBodyName(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_VI_4_1_1_CONTENT_SELECTOR, 0);
    }

    /**
     * Parse tender mediation body name value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderMediationBodyName(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_VI_4_1_2_CONTENT_SELECTOR, 0);
    }

    /**
     * Parse tender lots from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return list of lots or Null
     */
    private static List<ParsedTenderLot> parseTenderLots(final Element form) {
        final String sectionFooterClassName = "section-footer";

        Element lotElement = JsoupUtils.selectFirst(ANCIENT_SECTION_B_FIRST_ELEMENT_OF_FIRST_LOT_SELECTOR, form);
        if (lotElement == null || lotElement.className().equals(sectionFooterClassName)) {
            return null;
        }

        List<ParsedTenderLot> lots = new ArrayList<>();
        final String lotNumberInLotFormTitleText = "PERCEEL NR.";
        final String lotNumberAloneText = "PERCEEL NR.:";
        final String lotTitleAloneText = "TITEL:";
        final String cpvNumberTitle = "Hoofdcategorieën:";
        do {
            String lotFormTitle = lotElement.ownText();
            assert lotFormTitle.startsWith(lotNumberInLotFormTitleText);
            ParsedTenderLot lot = new ParsedTenderLot();
            if (lotFormTitle.startsWith(lotNumberAloneText)) {
                // https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
                // 8bdc23d990017ef638cf11fd616ff08e/pageId/D909A/huidigemenu/aankondigingen/cid/101938/cvp/join
                String[] lotNumberAndTitleRows = lotElement.html().split("<br>");
                for (int i = 0; i < lotNumberAndTitleRows.length; ++i) {
                    if (lotNumberAndTitleRows[i].startsWith(lotNumberAloneText)) {
                        lot.setLotNumber(lotNumberAndTitleRows[i].substring(lotNumberAloneText.length()).trim());
                    } else if (lotNumberAndTitleRows[i].startsWith(lotTitleAloneText)) {
                        lot.setTitle(lotNumberAndTitleRows[i].substring(lotTitleAloneText.length()).trim());
                    }
                }
            } else {
                int firstColonIndex = lotFormTitle.indexOf(':');
                int secondColonIndex = lotFormTitle.indexOf(':', firstColonIndex + 1);

                lot.setLotNumber(lotFormTitle.substring(lotNumberInLotFormTitleText.length(), firstColonIndex).trim())
                    .setTitle(lotFormTitle.substring((secondColonIndex != -1 ? secondColonIndex : firstColonIndex) + 1)
                        .trim());
                
            }
            lotElement = lotElement.nextElementSibling();
            // subsection 1
            if (lotElement.text().startsWith("1")) {
                lotElement = lotElement.nextElementSibling();
                String lotDescription = lotElement.ownText().trim();
                if (!lotDescription.isEmpty()) {
                    lot.setDescription(lotDescription);
                }
                lotElement = lotElement.nextElementSibling();
            }
            // subsection 2
            if (lotElement.text().startsWith("2")) {
                lotElement = lotElement.nextElementSibling();
                String[] subsection2Rows = lotElement.html().split("<br>");
                // sometimes CPVs are not filled (see
                // https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
                // 16a6feac8eec9956aa42e0f394d27b2e/pageId/D909A/huidigemenu/aankondigingen/cid/842375/cvp/join)
                if (subsection2Rows.length >= 2) {
                    lot.addCpv(new ParsedCPV()
                            .setIsMain(Boolean.TRUE.toString())
                            .setCode(subsection2Rows[1].substring(
                                    cpvNumberTitle.length(),
                                    subsection2Rows[1].indexOf('('))));
                    for (int i = 4; i < subsection2Rows.length; ++i) {
                        // CPVs are only on some rows. See
                        // https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
                        // facd367074bd0b897561740f52c05524/pageId/D909A/huidigemenu/aankondigingen/cid/962742/cvp/join
                        if (subsection2Rows[i].startsWith(cpvNumberTitle)) {
                            lot.addCpv(new ParsedCPV()
                                    .setIsMain(Boolean.FALSE.toString())
                                    .setCode(subsection2Rows[i].substring(
                                            cpvNumberTitle.length(), subsection2Rows[i].indexOf('('))));
                        }
                    }
                }
                lotElement = lotElement.nextElementSibling();
            }

            // subsection 3
            if (lotElement.text().startsWith("3")) {
                lotElement = lotElement.nextElementSibling();
                lotElement = lotElement.nextElementSibling();
            }

            // subsection 4
            if (lotElement.text().startsWith("4")) {
                lotElement = lotElement.nextElementSibling();

                Matcher m = Pattern.compile("Aanvang\\(dd/mm/jjjj\\): (?<start>\\d{2}/\\d{2}/\\d{4})"
                    + " Voltooiing\\(dd/mm/jjjj\\): (?<completion>\\d{2}/\\d{2}/\\d{4})").matcher(lotElement.text());
                
                if (m.find()) {
                    lot.setEstimatedStartDate(m.group("start")).setEstimatedCompletionDate(m.group("completion"));
                }
            }

            lots.add(lot);

            // move to the first element of next lot
            do {
                lotElement = lotElement.nextElementSibling();
            } while (lotElement != null && !lotElement.ownText().contains(lotNumberInLotFormTitleText)
                    && !lotElement.className().equals(sectionFooterClassName));

            if (lotElement == null || lotElement.className().equals(sectionFooterClassName)) {
                break;
            }
        } while (true);
        return lots;
    }
}
