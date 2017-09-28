package eu.digiwhist.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

import static eu.digiwhist.worker.cz.parsed.VVZTenderParserUtils.concatenateStrings;
import static eu.digiwhist.worker.cz.parsed.VVZTenderParserUtils.getCheckedInputValue;
import static eu.digiwhist.worker.cz.parsed.VVZTenderParserUtils.getFieldValue;

/**
 * Handler for Czech (under-the-threshold) Prior contract notice and Contract notice on Vestnik.
 */
abstract class VVZCzPriorAndContractNoticeHandler extends VVZCzFormsHandler {
    private static final Logger logger = LoggerFactory.getLogger(VVZCzPriorAndContractNoticeHandler.class);

    /**
     * Parses Form specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from Form
     */
    public static ParsedTender parseCommonPriorAndContractNoticeAttributes(final ParsedTender tender,
            final Document form) {
        ParsedTender parsedTender = tender;

        // SECTION I
        final Element sectionI = VVZTenderParser.getSectionI(form);

        // subsection I.1)
        parsedTender.addBuyer(parseBuyer(sectionI));

        // subsection I.2)
        parsedTender.setDocumentsLocation(VVZTenderParser.parseDocumentsLocation(sectionI))
                .setBidsRecipient(parseBidsRecipient(sectionI));

        // SECTION II
        final Element sectionII = VVZTenderParser.getSectionII(form);

        // subsection II.1.1)
        parsedTender.setTitle(VVZTenderParser.parseTenderTitle(sectionII));

        // subsection II.1.2)
        parsedTender.setSupplyType(VVZTenderParser.parseSupplyType(sectionII))
                .addCpv(parseCpvCode(sectionII))
                .setAddressOfImplementation(parseAddressOfImplementation(sectionII));

        // subsection II.1.3)
        parsedTender.setDescription(parseTenderDescription(sectionII));

        // subsection II.1.4)
        parsedTender.setAreVariantsAccepted(VVZTenderParser.parseAreVariantsAcceptedFromCheckbox(sectionII));

        // subsection II.1.6) and II.1.7) and excessive framework agreement justification from II.3.1)
        parsedTender = parseFrameworkAgreementAndDPSInfo(sectionII, parsedTender);

        // subsection II.1.8)
        parsedTender.addFunding(VVZTenderParser.parseEuFunding(sectionII));

        // subsection II.2.1)
        parsedTender.setEstimatedPrice(parseEstimatedPrice(sectionII));

        // subsection II.3.1) (without excessive framework agreement justification)
        parsedTender.setEstimatedDurationInMonths(parseEstimatedDurationInMonths(sectionII))
                .setEstimatedDurationInDays(parseEstimatedDurationInDays(sectionII))
                .setEstimatedStartDate(parseEstimatedStartDate(sectionII))
                .setEstimatedCompletionDate(parseEstimatedCompletionDate(sectionII));

        // subsection II.4.1)
        parsedTender.setHasLots(VVZTenderParser.parseHasLots(sectionII));

        // SECTION III
        final Element sectionIII = VVZTenderParser.getSectionIII(form);

        // subsection III.1.1)
        parsedTender.setNationalProcedureType(VVZTenderParser.parseProcedureType(sectionIII));

        // subsection III.2.1)
        parsedTender.setSelectionMethod(parseSelectionMethod(sectionIII));
        parsedTender.setAwardCriteria(parseAwardCriteria(sectionIII));

        // subsection III.2.2)
        parsedTender.setIsElectronicAuction(VVZTenderParser.parseIsElectronicAuction(sectionIII));

        // subsection III.3.1)
        parsedTender.setBidDeadline(parseBidDeadline(sectionIII));

        // subsection III.3.2)
        parsedTender.setEligibleBidLanguages(parseEligibleBidLanguages(sectionIII));

        // SECTION V
        final Element sectionV = VVZTenderParser.getCZSectionV(form);

        // subsection V.1)
        parsedTender.setAdditionalInfo(VVZTenderParser.parseAdditionalInfo(sectionV));

        return tender;
    }

    // ---------------------------------
    // CZ SUBSECTION I.2)
    // ---------------------------------

    /**
     * Parses bids recipient info.
     *
     * @param sectionI
     *         section I hmtl
     *
     * @return bids recipient
     */
    private static ParsedBody parseBidsRecipient(final Element sectionI) {
        final String bidsRecipientCheckbox = getCheckedInputValue(sectionI, "Body\\.AddressFurtherInfoIdem");
        if (bidsRecipientCheckbox != null) {
            switch (bidsRecipientCheckbox) {
                case "TENDER_ELECTRONICALY":
                    final String bidsRecipientUrl = getFieldValue(sectionI, "Body\\.UrlParticipation");
                    return new ParsedBody().setAddress(
                            new ParsedAddress().setUrl(bidsRecipientUrl));
                case "ADDITIONAL_ADDRESS":
                    return new ParsedBody().setAddress(
                            new ParsedAddress().setRawAddress(getFieldValue(sectionI, ".*AddressAdditional$")));
                default:
                    break;
            }
        }
        return null;
    }

    // ---------------------------------
    // CZ SUBSECTION II.1.6)
    // ---------------------------------

    /**
     * Parses framework agreement and DPS info.
     *
     * @param sectionII
     *         section II html
     * @param tender
     *         tender to be updated
     *
     * @return tender updated with framework agreement and DPS info
     */
    private static ParsedTender parseFrameworkAgreementAndDPSInfo(final Element sectionII, final ParsedTender tender) {
        ParsedTender parsedTender = tender;
        final String noticeInvolvesValue = parseNoticeInvolvesValues(sectionII);

        if (noticeInvolvesValue != null) {
            if (noticeInvolvesValue.equalsIgnoreCase("CONCLUSION_FRAMEWORK_AGREEMENT")) {
                parsedTender.setIsFrameworkAgreement(Boolean.TRUE.toString());
                parsedTender = parseFrameworkAgreementDetails(sectionII, parsedTender);
            } else if (noticeInvolvesValue.equalsIgnoreCase("CONTRACTS_DPS")) {
                parsedTender.setIsDps(Boolean.TRUE.toString());
            }
        }
        return parsedTender;
    }

    // ---------------------------------
    // CZ SUBSECTION II.1.7)
    // ---------------------------------

    /**
     * Parses details about framework agreement.
     *
     * @param sectionII
     *         section II html
     * @param tender
     *         tender to be updated
     *
     * @return tender updated with framework agreement info
     */
    private static ParsedTender parseFrameworkAgreementDetails(final Element sectionII, final ParsedTender tender) {
        final String numberOfParticipantsRadio = getCheckedInputValue(sectionII, ".*\\.SingleOperator$");
        if (numberOfParticipantsRadio != null) {
            // if SINGLE_OPERATOR is checked, set number of participants to 1
            if (numberOfParticipantsRadio.equalsIgnoreCase("SINGLE_OPERATOR")) {
                tender.setMaxFrameworkAgreementParticipants(Integer.toString(1));
            } else {
                // parse number of participants from the text box
                tender.setMaxFrameworkAgreementParticipants(getFieldValue(sectionII, ".*\\.SeveralOperators$"));
            }
            // subsection II.3.1)
            tender.setExcessiveFrameworkAgreementJustification(
                    parseExcessiveFrameworkAgreementJustification(sectionII));
        }
        return tender;
    }

    // ---------------------------------
    // CZ SUBSECTION II.3.1) and II.5.5)
    // ---------------------------------

    /**
     * Parses estimated duration in months.
     *
     * @param root
     *         root element for parsing
     *
     * @return estimated duration in months
     */
    private static String parseEstimatedDurationInMonths(final Element root) {
        return getFieldValue(root, ".*DurationMonth$");
    }

    /**
     * Parses estimated duration in days.
     *
     * @param root
     *         root element for parsing
     *
     * @return estimated duration in days
     */
    private static String parseEstimatedDurationInDays(final Element root) {
        return getFieldValue(root, ".*DurationDays$");
    }

    /**
     * Parses estimated start date.
     *
     * @param root
     *         root element for parsing
     *
     * @return estimated start day
     */
    private static String parseEstimatedStartDate(final Element root) {
        return getFieldValue(root, ".*TimeFrameFrom");
    }

    /**
     * Parses estimated completion date.
     *
     * @param root
     *         root element for parsing
     *
     * @return estimated completion date
     */
    private static String parseEstimatedCompletionDate(final Element root) {
        return getFieldValue(root, ".*TimeFrameTo");
    }

    /**
     * Parses excessive framework agreement justification.
     *
     * @param root
     *         root element for parsing
     *
     * @return excessive framework agreement justification
     */
    private static String parseExcessiveFrameworkAgreementJustification(final Element root) {
        return VVZTenderParserUtils.getFieldValue(root, ".*Justification$");
    }

    // ---------------------------------
    // CZ SUBSECTION II.5)
    // ---------------------------------

    /**
     * Parses common attributes for lots.
     *
     * @param lotHtml
     *         lot html
     * @param lotPositionOnPage
     *         lot position on page
     *
     * @return parsed tender lot
     */
    static ParsedTenderLot parseCommonLotAttributes(final Element lotHtml, final int lotPositionOnPage) {
        final ParsedTenderLot lot = new ParsedTenderLot().setPositionOnPage(Integer.toString(lotPositionOnPage));

        //subsection II.5.1)
        lot.setTitle(VVZTenderParser.parseLotTitle(lotHtml))
                .setLotNumber(VVZTenderParser.parseLotNumber(lotHtml));

        // subsection II.5.2)
        lot.setDescription(VVZTenderParser.parseLotDescription(lotHtml));

        // subsection II.5.3)
        lot.addCpv(parseCpvCode(lotHtml)).setAddressOfImplementation(parseLotAddressOfImplementation(lotHtml));

        // subsection II.5.4)
        lot.setEstimatedPrice(parseEstimatedPrice(lotHtml));

        // subsection II.5.5)
        lot.setEstimatedDurationInMonths(parseEstimatedDurationInMonths(lotHtml))
                .setEstimatedDurationInDays(parseEstimatedDurationInDays(lotHtml))
                .setEstimatedStartDate(parseEstimatedStartDate(lotHtml))
                .setEstimatedCompletionDate(parseEstimatedCompletionDate(lotHtml));

        return lot;
    }

    // ---------------------------------
    // CZ SUBSECTION III.3.1) and II.5.7)
    // ---------------------------------

    /**
     * Parses bid deadline.
     *
     * @param root
     *         root element for parsing
     *
     * @return bid deadline
     */
    static String parseBidDeadline(final Element root) {
        final String deadlineDate = getFieldValue(root, ".*\\.DateReceiptTenders$");
        final String deadlineTime = getFieldValue(root, ".*\\.TimeReceiptTenders$");
        return concatenateStrings(deadlineDate, deadlineTime);
    }

    // ---------------------------------
    // CZ SUBSECTION III.3.2)
    // ---------------------------------

    /**
     * Parses bid eligible languages.
     *
     * @param sectionIII
     *         section III html
     *
     * @return eligible bid languages
     */
    private static List<String> parseEligibleBidLanguages(final Element sectionIII) {
        final String czechBidsOnly = VVZTenderParserUtils.getCheckedInputValue(sectionIII, ".*\\.CzechLanguageOnly$");
        if (czechBidsOnly == null) {
            return null;
        }

        final List<String> bidLanguages = new ArrayList<>();
        bidLanguages.add("CS");

        if (czechBidsOnly.equalsIgnoreCase("false")) {
            bidLanguages.add(VVZTenderParserUtils.getFieldValue(sectionIII, ".*\\.Languages$"));
        }
        return bidLanguages;
    }
}
