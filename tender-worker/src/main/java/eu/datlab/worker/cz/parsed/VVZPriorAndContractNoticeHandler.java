package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.apache.commons.lang3.BooleanUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import static eu.datlab.worker.cz.parsed.VVZTenderParserUtils.concatenateStrings;
import static eu.datlab.worker.cz.parsed.VVZTenderParserUtils.getCheckedInputValue;
import static eu.datlab.worker.cz.parsed.VVZTenderParserUtils.getFieldValue;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Handler for Contract Notice and Prior contract notice on Vestnik.
 */
abstract class VVZPriorAndContractNoticeHandler extends VVZEuFormsHandler {

    /**
     * Parses Contract notice and Prior contract notice specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page
     *
     * @return updated tender object with data parsed from Contract Notice form
     */
    static ParsedTender parseCommonPriorAndContractNoticeAttributes(final ParsedTender tender, final Document form) {
        ParsedTender parsedTender = tender;

        final Element sectionI = VVZTenderParser.getSectionI(form);
        final Element sectionII = VVZTenderParser.getSectionII(form);
        final Element sectionIII = VVZTenderParser.getSectionIII(form);
        final Element sectionIV = VVZTenderParser.getSectionIV(form);
        final Element sectionVI = VVZTenderParser.getSectionVI(form);

        // SECTION I

        // subsection I.1) I.2) and I.3)
        parsedTender = parseSectionICommonSubsections(sectionI, parsedTender);

        // SECTION II

        // subsection II.1.1)
        parsedTender.setTitle(VVZTenderParser.parseTenderTitle(sectionII))
                // subsection II.1.2)
                .setCpvs(parseTenderCPVCodes(sectionII))
                // subsection II.1.3)
                .setSupplyType(VVZTenderParser.parseSupplyType(sectionII))
                // subsection II.1.4)
                .setDescription(parseTenderDescription(sectionII))
                // subsection II.1.5)
                .setEstimatedPrice(parseTenderEstimatedPrice(sectionII))
                // subsection II.1.6)
                .setHasLots(VVZTenderParser.parseHasLots(sectionII));

        // SECTION III

        // subsection III.1.1)
        parsedTender.setPersonalRequirements(parsePersonalRequirements(sectionIII))
                // subsection III.1.2)
                .setEconomicRequirements(parseEconomicRequirements(sectionIII))
                // subsection III.1.3)
                .setTechnicalRequirements(parseTechnicalRequirements(sectionIII));

        // SECTION IV

        // subsection IV.1.1)
        parsedTender.setNationalProcedureType(VVZTenderParser.parseProcedureType(sectionIV));

        // subsection IV.1.3)
        parsedTender = parseFrameworkAgreementInfo(sectionIV, parsedTender);
        parsedTender.setIsDps(parseIsDPS(sectionIV))
                // subsection IV.1.6)
                .setIsElectronicAuction(VVZTenderParser.parseIsElectronicAuction(sectionIV))
                // subsection IV.1.8)
                .setIsCoveredByGpa(parseIsCoveredByGPA(sectionIV))
                // subsection IV.2.2)
                .setBidDeadline(parseBidDeadline(sectionIV))
                // subsection IV.2.4)
                .setEligibleBidLanguages(parseEligibleBidLanguages(sectionIV));

        // SECTION VI

        // subsection VI.2)
        parsedTender.setIsEInvoiceAccepted(parseIsEInvoiceAccepted(sectionVI))
                // subsection VI.4.1)
                .setAppealBodyName(parseAppealBodyName(sectionVI))
                // subsection VI.4.2)
                .setMediationBodyName(parseMediationBodyName(sectionVI));

        // subsection VI.3)
        parsedTender.setAdditionalInfo(VVZTenderParser.parseAdditionalInfo(sectionVI));

        return parsedTender;
    }

    // =================================
    // SECTION I
    // =================================

    // parses subsection I.1), I.2) and I.3)

    /**
     * Parses common fields for prior and contract notices from section I (subsections I.1, I.2, I.3) - buyers,
     * central procurement, joint procurement, documents location, further information provider and bids recipient.
     *
     * @param sectionI
     *         section I html
     * @param tender
     *         tender to be updated
     *
     * @return tender with parsed info about buyers, central procurement, joint procurement, documents location,
     * further information provider and bids recipient
     */
    static ParsedTender parseSectionICommonSubsections(final Element sectionI, final ParsedTender tender) {
        return tender.setBuyers(parseBuyers(sectionI))
                .setIsCentralProcurement(parseCentralProcurement(sectionI))
                .setIsJointProcurement(parseJointProcurement(sectionI))
                .setDocumentsLocation(VVZTenderParser.parseDocumentsLocation(sectionI))
                .setFurtherInformationProvider(parseFurtherInformationProvider(sectionI))
                .setBidsRecipient(parseBidsRecipient(sectionI));
    }

    // ---------------------------------
    // SUBSECTION I.3)
    // ---------------------------------

    /**
     * Parses further information provider.
     *
     * @param root
     *         html for parsing
     *
     * @return further information provider
     */
    private static ParsedBody parseFurtherInformationProvider(final Element root) {
        final String furtherInformationProviderCheckbox = getCheckedInputValue(root, "Body\\.AddressFurtherInfoIdem");
        if (furtherInformationProviderCheckbox != null && furtherInformationProviderCheckbox.equalsIgnoreCase(
                "ANOTHER")) {
            return VVZTenderParser.parseBody(root.select("div#Body_AddressFurtherInfo").first());
        }
        return null;
    }

    /**
     * Parses bids recipient.
     *
     * @param root
     *         html for parsing
     *
     * @return bids recipient
     */
    private static ParsedBody parseBidsRecipient(final Element root) {
        final String bidsUrlCheckbox = getCheckedInputValue(root, "Body\\.WithUrlParticipation");
        if (bidsUrlCheckbox != null && bidsUrlCheckbox.equalsIgnoreCase("true")) {
            return new ParsedBody().setAddress(
                    new ParsedAddress().setUrl(getFieldValue(root, "Body\\.UrlParticipation")));
        }

        final String bidsRecipientCheckbox = getCheckedInputValue(root, "Body\\.AddressParticipationIdem");
        if (bidsRecipientCheckbox != null && bidsRecipientCheckbox.equalsIgnoreCase("ANOTHER")) {
            return VVZTenderParser.parseBody(root.select("div#Body_AddressParticipation").first());
        }
        return null;
    }

    // =================================
    // SECTION II
    // =================================

    // ---------------------------------
    // SUBSECTION II.1.5)
    // ---------------------------------

    /**
     * Parses tender estimated price.
     *
     * @param sectionII
     *         section II html
     *
     * @return tender estimated price
     */
    private static ParsedPrice parseTenderEstimatedPrice(final Element sectionII) {
        final Element tenderEstimatedPriceDiv = sectionII.select("div#Contract_EstimatedTotal").first();
        return parseEstimatedPrice(tenderEstimatedPriceDiv);
    }

    // ---------------------------------
    // SUBSECTION II.2)
    // ---------------------------------

    /**
     * Parses lots.
     *
     * @param form
     *         form html
     *
     * @return lots
     */
    static List<ParsedTenderLot> parseContractNoticeLots(final Document form) {
        List<ParsedTenderLot> lots = new ArrayList<>();

        Elements lotsHtml = getLotsHtmls(form);
        for (Element lotHtml : lotsHtml) {
            ParsedTenderLot parsedLot = parseCommonLotAttributes(lotHtml, lotsHtml.indexOf(lotHtml) + 1)
                    // subsection II.2.10)
                    .setAreVariantsAccepted(parseAreLotVariantsAcceptedFromRadio(lotHtml))
                    // subsection II.2.11)
                    .setHasOptions(parseHasLotOptionsFromRadio(lotHtml));
            parsedLot = parseLotEnvisagedCandidatesCountInfo(lotHtml, parsedLot);
            lots.add(parsedLot);
        }
        return lots;
    }

    /**
     * Gets lots html segments.
     *
     * @param form
     *         form html
     *
     * @return lots html segments
     */
    static Elements getLotsHtmls(final Document form) {
        final Element sectionII = VVZTenderParser.getSectionII(form);
        return sectionII.select("div[model~=.*ObjectDescrList\\[\\d+\\]$]");
    }

    /**
     * Parses common attributes for lot.
     *
     * @param lotHtml
     *         lot html
     * @param lotPositionOnPage
     *         lot position on page
     *
     * @return lot with parsed common attributes (lot number, position on page, CPVs, etc.)
     */
    static ParsedTenderLot parseCommonLotAttributes(final Element lotHtml, final int lotPositionOnPage) {
        return new ParsedTenderLot().setTitle(VVZTenderParser.parseLotTitle(lotHtml))
                .setLotNumber(VVZTenderParser.parseLotNumber(lotHtml))
                .setPositionOnPage(Integer.toString(lotPositionOnPage))
                // subsection II.2.2)
                .setCpvs(VVZTenderParser.parseCPVCodes(lotHtml))
                // subsection II.2.3)
                .setAddressOfImplementation(parseAddressOfImplementation(lotHtml))
                // subsection II.2.4)
                .setDescription(VVZTenderParser.parseLotDescription(lotHtml))
                // subsection II.2.5)
                .setAwardCriteria(parseLotAwardCriteria(lotHtml))
                // subsection II.2.6)
                .setEstimatedPrice(parseLotEstimatedPrice(lotHtml))
                // subsection II.2.7)
                .setEstimatedDurationInMonths(parseLotEstimatedDurationInMonths(lotHtml))
                .setEstimatedDurationInDays(parseLotEstimatedDurationInDays(lotHtml))
                .setEstimatedStartDate(parseLotEstimatedStartDate(lotHtml))
                .setEstimatedCompletionDate(parseLotEstimatedCompletionDate(lotHtml))
                // subsection II.2.13)
                .addFunding(VVZTenderParser.parseEuFunding(lotHtml));
    }

    // ---------------------------------
    // SUBSECTION II.2.6)
    // ---------------------------------

    /**
     * Parses lot estimated price.
     *
     * @param lotHtml
     *         lot html
     *
     * @return lot estimated price
     */
    private static ParsedPrice parseLotEstimatedPrice(final Element lotHtml) {
        return parseEstimatedPrice(lotHtml.select("div[id~=.*EstimatedValue]").first());
    }

    // ---------------------------------
    // SUBSECTION II.2.9)
    // ---------------------------------

    /**
     * Parses envisaged candidates count info.
     *
     * @param lotHtml
     *         lot html
     * @param lot
     *         lot to be updated
     *
     * @return updated lot with envisaged candidates count info
     */
    private static ParsedTenderLot parseLotEnvisagedCandidatesCountInfo(final Element lotHtml,
            final ParsedTenderLot lot) {
        return lot.setEnvisagedCandidatesCount(parseLotEnvisagedCandidatesCount(lotHtml))
                .setEnvisagedMinCandidatesCount(parseLotEnvisagedMinCandidatesCount(lotHtml))
                .setEnvisagedMaxCandidatesCount(parseLotEnvisagedMaxCandidatesCount(lotHtml))
                .setLimitedCandidatesCountCriteria(parseLotLimitedCandidatesCountCriteria(lotHtml));
    }

    /**
     * Parses envisaged candidates count.
     *
     * @param lotHtml
     *         lot html
     *
     * @return envisaged candidates count
     */
    private static String parseLotEnvisagedCandidatesCount(final Element lotHtml) {
        return VVZTenderParserUtils.getFieldValue(lotHtml, ".*LimitCandidateEnvisaged$");
    }

    /**
     * Parses envisaged minimum candidates count.
     *
     * @param lotHtml
     *         lot html
     *
     * @return envisaged minimum candidates count
     */
    private static String parseLotEnvisagedMinCandidatesCount(final Element lotHtml) {
        return VVZTenderParserUtils.getFieldValue(lotHtml, ".*LimitCandidateMin$");
    }

    /**
     * Parses envisaged maximum candidates count.
     *
     * @param lotHtml
     *         lot html
     *
     * @return envisaged maximum candidates count
     */
    private static String parseLotEnvisagedMaxCandidatesCount(final Element lotHtml) {
        return VVZTenderParserUtils.getFieldValue(lotHtml, ".*LimitCandidateMax$");
    }

    /**
     * Parses limited candidates count criteria.
     *
     * @param lotHtml
     *         lot html
     *
     * @return limited candidates count criteria
     */
    private static String parseLotLimitedCandidatesCountCriteria(final Element lotHtml) {
        return VVZTenderParserUtils.getFieldValue(lotHtml, ".*LimitCandidateDesc$");
    }

    // ---------------------------------
    // SUBSECTION II.2.10)
    // ---------------------------------

    /**
     * Parses whether lot variants are accepted.
     *
     * @param lotHtml
     *         lot html
     *
     * @return true if lot variants are accepted
     */
    private static String parseAreLotVariantsAcceptedFromRadio(final Element lotHtml) {
        return VVZTenderParserUtils.getCheckedInputValue(lotHtml, ".*AcceptedVariants");
    }

    // =================================
    // SECTION III
    // =================================

    // ---------------------------------
    // SUBSECTION III.1.1)
    // ---------------------------------

    /**
     * Parses personal requirements.
     *
     * @param sectionIII
     *         section III html
     *
     * @return person requirements
     */
    private static String parsePersonalRequirements(final Element sectionIII) {
        return getFieldValue(sectionIII, "LeftI\\.Suitability");
    }

    // ---------------------------------
    // SUBSECTION III.1.2)
    // ---------------------------------

    /**
     * Parses economic requirements.
     *
     * @param sectionIII
     *         section III html
     *
     * @return economic requirements
     */
    private static String parseEconomicRequirements(final Element sectionIII) {
        final Boolean areEconomicCriteriaInSpecification = VVZTenderParserUtils.isInputFieldChecked(sectionIII,
                "LeftI\\.EconomicCriteria");
        if (areEconomicCriteriaInSpecification == null || !areEconomicCriteriaInSpecification) {
            final String economicCriteria = getFieldValue(sectionIII, "LeftI\\.FinancialInfo");
            final String economicCriteriaMinLevel = getFieldValue(sectionIII, "LeftI\\.FinancialMinLevel");
            if (economicCriteria == null) {
                return economicCriteriaMinLevel;
            }
            if (economicCriteriaMinLevel == null) {
                return economicCriteria;
            }
            return economicCriteria + economicCriteriaMinLevel;
        }
        return null;
    }

    // ---------------------------------
    // SUBSECTION III.1.3)
    // ---------------------------------

    /**
     * Parses technical requirements.
     *
     * @param sectionIII
     *         section III html
     *
     * @return technical requirements
     */
    private static String parseTechnicalRequirements(final Element sectionIII) {
        final Boolean areTechnicalCriteriaInSpecification = VVZTenderParserUtils.isInputFieldChecked(sectionIII,
                "LeftI\\.TechnicalCriteria");
        if (areTechnicalCriteriaInSpecification == null || !areTechnicalCriteriaInSpecification) {
            final String technicalCriteria = getFieldValue(sectionIII, "LeftI\\.ProfessionalInfo");
            final String technicalCriteriaMinLevel = getFieldValue(sectionIII, "LeftI\\.ProfessionalMinLevel");
            return VVZTenderParserUtils.concatenateStrings(technicalCriteria, technicalCriteriaMinLevel);
        }
        return null;
    }

    // ---------------------------------
    // SUBSECTION IV.1.3)
    // ---------------------------------

    /**
     * Parses framework agreement info.
     *
     * @param sectionIV
     *         section IV html
     * @param parsedTender
     *         tender to be updated
     *
     * @return tender updated with framework agreement info
     */
    static ParsedTender parseFrameworkAgreementInfo(final Element sectionIV, final ParsedTender parsedTender) {
        final String isFrameworkAgreement = parseIsFrameworkAgreement(sectionIV);

        parsedTender.setIsFrameworkAgreement(isFrameworkAgreement);

        // in case of framework agreement, parse number of participants
        if (BooleanUtils.toBoolean(isFrameworkAgreement)) {
            parsedTender.setExcessiveFrameworkAgreementJustification(getFieldValue(sectionIV, ".*\\.Justification"));

            final String numberOfParticipantsRadio = getCheckedInputValue(sectionIV, ".*\\.OperatorType");
            if (numberOfParticipantsRadio != null) {
                // if SINGLE_OPERATOR is checked, set number of participants to 1
                if (numberOfParticipantsRadio.equalsIgnoreCase("SINGLE_OPERATOR")) {
                    parsedTender.setMaxFrameworkAgreementParticipants(Integer.toString(1));
                } else {
                    // parse number of participants from the text box
                    parsedTender.setMaxFrameworkAgreementParticipants(
                            getFieldValue(sectionIV, ".*\\.SeveralOperatorsNumber"));
                }
            }
        }
        return parsedTender;
    }

    // ---------------------------------
    // SUBSECTION IV.2.2)
    // ---------------------------------

    /**
     * Parses bid deadline.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return bid deadline
     */
    private static String parseBidDeadline(final Element sectionIV) {
        final String deadlineDate = getFieldValue(sectionIV, ".*\\.TimeLimitDate$");
        final String deadlineTime = getFieldValue(sectionIV, ".*\\.TimeLimitTime$");
        return concatenateStrings(deadlineDate, deadlineTime);
    }

    // ---------------------------------
    // SUBSECTION IV.2.4)
    // ---------------------------------

    /**
     * Parses bid eligible languages.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return bid eligible langueages
     */
    private static List<String> parseEligibleBidLanguages(final Element sectionIV) {
        return sectionIV.select("select[name~=.*\\.LanguagesList\\[\\d+\\]$] > option[selected]")
                .stream()
                .filter(bidLang -> !bidLang.attr("value").isEmpty())
                .map(bidLang -> bidLang.attr("value"))
                .collect(Collectors.toList());
    }

    // ---------------------------------
    // SUBSECTION IV.2.6)
    // ---------------------------------

    /**
     * Parses award deadline.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return award deadline
     */
    static String parseAwardDeadline(final Element sectionIV) {
        return VVZTenderParserUtils.getFieldValue(sectionIV, ".*\\.TenderValidDate$");
    }

    /**
     * Parses award deadline duration.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return award deadline duration
     */
    static String parseAwardDeadlineDuration(final Element sectionIV) {
        return VVZTenderParserUtils.getFieldValue(sectionIV, ".*\\.TenderValidDuration$");
    }

    // =================================
    // SECTION VI
    // =================================

    // ---------------------------------
    // SUBSECTION VI.2)
    // ---------------------------------

    /**
     * Parses whether e-invoice is accepted.
     *
     * @param sectionVI
     *         section VI html
     *
     * @return true if e-invoice is accepted
     */
    private static String parseIsEInvoiceAccepted(final Element sectionVI) {
        return BooleanUtils.toStringTrueFalse(VVZTenderParserUtils.isInputFieldChecked(sectionVI, ".*\\.EInvoicing$"));
    }

    // *********************************
    // COMMON UTILS METHODS
    // *********************************

    /**
     * Parses estimated price.
     *
     * @param priceDiv
     *         html with price info
     *
     * @return estimated price
     */
    private static ParsedPrice parseEstimatedPrice(final Element priceDiv) {
        final String netAmount = VVZTenderParserUtils.getFieldValue(priceDiv, ".*ValueFrom");
        final String currency = VVZTenderParserUtils.getSelectedOptionValue(priceDiv, ".*Currency");
        if (netAmount != null || currency != null) {
            return new ParsedPrice().setNetAmount(netAmount).setCurrency(currency);
        }
        return null;
    }
}
