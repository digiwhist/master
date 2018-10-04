package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.apache.commons.lang.StringUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * Handler for parsing form F20 - Modification notice.
 */
final class VVZFormF20Handler extends VVZEuFormsHandler {
    private static final Logger logger = LoggerFactory.getLogger(VVZFormF20Handler.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZFormF20Handler() {
        throw new AssertionError();
    }

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
    public static ParsedTender parseFormAttributes(final ParsedTender tender, final Document form) {
        ParsedTender parsedTender = tender;

        final Element sectionI = VVZTenderParser.getSectionI(form);
        final Element sectionII = VVZTenderParser.getSectionII(form);
        final Element sectionIV = VVZTenderParser.getSectionIV(form);
        final Element sectionVI = VVZTenderParser.getSectionVI(form);
        final Element sectionVII = getSectionVII(form);

        // parse info about publication of related original form that is being corrected
        tender.addPublication(VVZTenderParser.parseRelatedOriginalPublicationFromHeader(form));

        // SECTION I

        // subsection I.1
        parsedTender.setBuyers(parseBuyers(sectionI));

        // SECTION II

        // subsection II.1.1
        parsedTender.setTitle(VVZTenderParser.parseTenderTitle(sectionII));

        // subsection II.1.3
        parsedTender.setSupplyType(VVZTenderParser.parseSupplyType(sectionII));

        // subsection II.2.13
        parsedTender.addFunding(VVZTenderParser.parseEuFunding(sectionII));

        // SECTION IV

        // subsection IV.2.1
        parsedTender.addPublication(parsePreviousTedPublication(sectionIV));

        // SECTION V and VII.1.6 and VII.1.7
        parsedTender.addLot(parseLotAward(form));

        // SECTION VI

        // subsection VI.3)
        parsedTender.setAdditionalInfo(VVZTenderParser.parseAdditionalInfo(sectionVI));

        // subsection VI.4.1
        parsedTender.setAppealBodyName(parseAppealBodyName(sectionVI));

        // subsection VI.4.2
        parsedTender.setMediationBodyName(parseMediationBodyName(sectionVI));

        // SECTION VII

        // subsection VII.1.1
        parsedTender.setCpvs(parseCPVs(sectionVII));

        // subsection VII.1.3
        parsedTender.setAddressOfImplementation(parseAddressOfImplementation(sectionVII));

        // VII.1.4
        parsedTender.setDescription(VVZTenderParser.parseLotDescription(sectionVII));

        // VII.1.5
        parsedTender.setEstimatedDurationInMonths(parseLotEstimatedDurationInMonths(sectionVII))
                .setEstimatedDurationInDays(parseLotEstimatedDurationInDays(sectionVII))
                .setEstimatedStartDate(parseLotEstimatedStartDate(sectionVII))
                .setEstimatedCompletionDate(parseLotEstimatedCompletionDate(sectionVII));
        parsedTender.setExcessiveFrameworkAgreementJustification(
                parseExcessiveFrameworkAgreementJustification(sectionVII));

        // VII.2.2
        parsedTender = parseModificationReasonInfo(sectionVII, parsedTender);

        // VII.2.3
        parsedTender.setFinalPrice(parseTenderNewFinalPrice(sectionVII));

        return parsedTender;
    }

    /**
     * Gets section VII html.
     *
     * @param form
     *         form html
     *
     * @return section VII html
     */
    private static Element getSectionVII(final Document form) {
        return form.select("div#Modification").first();
    }

    // =================================
    // SECTION II
    // =================================

    // ---------------------------------
    // SUBSECTION II.2.7)
    // ---------------------------------

    /**
     * Parses excessive framework agreement justification.
     *
     * @param sectionII
     *         section II html
     *
     * @return excessive framework agreement justification
     */
    private static String parseExcessiveFrameworkAgreementJustification(final Element sectionII) {
        return sectionII.select("textarea[name~=.*Justification$]").text();
    }

    // =================================
    // SECTION V and VII
    // =================================

    /**
     * Parses lot award.
     *
     * @param form
     *         form html
     *
     * @return lot with award info
     */
    private static ParsedTenderLot parseLotAward(final Document form) {
        final Element lotHtml = VVZTenderParser.getLotsAwardsHtmls(form).first();
        final Element sectionVII = getSectionVII(form);

        ParsedTenderLot parsedLot = new ParsedTenderLot();

        // section V (section start)
        parsedLot.setContractNumber(VVZTenderParser.parseLotAwardContractNumber(lotHtml))
                .setLotNumber(VVZTenderParser.parseLotAwardNumber(lotHtml))
                .setTitle(VVZTenderParser.parseLotAwardTitle(lotHtml));

        // parse awarded lot info
        // subsection V.2)

        // subsection V.2.1)
        parsedLot.setContractSignatureDate(VVZTenderParser.parseLotAwardContractSignatureDate(lotHtml));

        // parse bid info (VII.1.6, VII.1.7)
        parsedLot.addBid(parseLotAwardWinningBid(sectionVII));

        return parsedLot;
    }

    /**
     * Parses lot winning bid.
     *
     * @param lotAwardHtml
     *         lot award html
     *
     * @return lot winning bid
     */
    private static ParsedBid parseLotAwardWinningBid(final Element lotAwardHtml) {
        ParsedBid winningBid = new ParsedBid().setIsWinning(Boolean.TRUE.toString());

        // subsection VII.1.6)
        winningBid.setPrice(parseLotAwardBidPrice(lotAwardHtml));

        // subsection VII.1.7)
        winningBid.setBidders(VVZTenderParser.parseLotAwardWinners(lotAwardHtml));

        // subsection VII.1.6-7)
        winningBid.setIsConsortium(VVZTenderParser.parseLotAwardBidIsConsortium(lotAwardHtml));

        return winningBid;
    }

    // ---------------------------------
    // SUBSECTION VII.1.1) and VII.1.2)
    // ---------------------------------

    /**
     * Parses CPVs.
     *
     * @param root
     *         html segment for parsing
     *
     * @return CPVs
     */
    private static List<ParsedCPV> parseCPVs(final Element root) {
        //final List<ParsedCPV> parsedCPVs = new ArrayList<>();

        final Element mainCpvDiv = root.select("div#Modification_CpvMain").first();
        //final Elements cpvDivs = root.select("div[model~=.*CpvAdditionalList\\[\\d+\\]$]");

        return VVZTenderParser.parseCPVCodes(mainCpvDiv);

        //for (Element cpvDiv : cpvDivs) {
        //    parsedCPVs.addAll(VVZTenderParser.parseCPVCodes(cpvDiv));
        //}
        //return parsedCPVs;
    }

    // ---------------------------------
    // SUBSECTION VII.1.6)
    // ---------------------------------

    /**
     * Parses winning bid price.
     *
     * @param lotHtml
     *         lot html
     *
     * @return winning bid price
     */
    private static ParsedPrice parseLotAwardBidPrice(final Element lotHtml) {
        final String netAmount = VVZTenderParserUtils.getFieldValue(lotHtml, ".*\\.ValueTotal\\.ValueFrom$");
        final String currency = VVZTenderParserUtils.getSelectedOptionValue(lotHtml, ".*\\.ValueTotal\\.Currency$");

        if (StringUtils.isNotEmpty(netAmount)) {
            return new ParsedPrice().setNetAmount(netAmount).setCurrency(currency);
        }
        return null;
    }

    // ---------------------------------
    // SUBSECTION VII.2.2)
    // ---------------------------------

    /**
     * Parses modification reason and description.
     *
     * @param sectionVII
     *         section VII html
     * @param tender
     *         tender to be updated
     *
     * @return tender with parsed modification reason and description
     */
    private static ParsedTender parseModificationReasonInfo(final Element sectionVII, final ParsedTender tender) {
        final String modificationReason = VVZTenderParserUtils.getCheckedInputValue(sectionVII, ".*\\.ModifyReason$");

        if (modificationReason != null) {
            String modificationReasonDescription = null;
            switch (modificationReason) {
                case "ADDITIONAL_NEED":
                    modificationReasonDescription = parseAdditionalNeedReasonDescription(sectionVII);
                    break;
                case "UNFORESEEN_CIRCUMSTANCE":
                    modificationReasonDescription = parseUnforseenCircumstanceReasonDescription(sectionVII);
                    break;
                default:
                    break;
            }
            tender.setModificationReason(modificationReason)
                    .setModificationReasonDescription(modificationReasonDescription);
        }
        return tender;
    }

    /**
     * Parses modification reason description for additional need reason.
     *
     * @param sectionVII
     *         section VII html
     *
     * @return modification reason description for additional need reason
     */
    private static String parseAdditionalNeedReasonDescription(final Element sectionVII) {
        return VVZTenderParserUtils.getFieldValue(sectionVII, ".*\\.UnforseenCircumstance_Additional$");
    }

    /**
     * Parses modification reason description for unforseen circumstances reason.
     *
     * @param sectionVII
     *         section VII html
     *
     * @return modification reason description for unforseen circumstances reason
     */
    private static String parseUnforseenCircumstanceReasonDescription(final Element sectionVII) {
        return VVZTenderParserUtils.getFieldValue(sectionVII, ".*\\.UnforseenCircumstance_Circumstance$");
    }

    // ---------------------------------
    // SUBSECTION VII.2.3)
    // ---------------------------------

    /**
     * Parses tender new final price.
     *
     * @param sectionVII
     *         section VII html
     *
     * @return tender new final price
     */
    private static ParsedPrice parseTenderNewFinalPrice(final Element sectionVII) {
        final String netAmount = VVZTenderParserUtils.getFieldValue(sectionVII, ".*\\.ValueTotalAfter\\.ValueFrom$");
        final String currency = VVZTenderParserUtils.getSelectedOptionValue(sectionVII,
                ".*\\.ValueTotalAfter\\.Currency$");

        if (StringUtils.isNotEmpty(netAmount)) {
            return new ParsedPrice().setNetAmount(netAmount).setCurrency(currency);
        }
        return null;
    }
}
