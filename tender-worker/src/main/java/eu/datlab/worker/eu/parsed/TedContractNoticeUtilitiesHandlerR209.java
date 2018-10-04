package eu.datlab.worker.eu.parsed;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.dl.dataaccess.dto.parsed.ParsedDocument;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * Parser for TED notice form with version R2.0.9.S01.E01 or newer.
 *
 * @author Tomas Mrazek
 */
public final class TedContractNoticeUtilitiesHandlerR209 {
    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedContractNoticeUtilitiesHandlerR209() {
        throw new AssertionError();
    }

    /**
     * Parses contract notice form specific data.
     *
     * @param parsedTender
     *         parsed tender
     * @param document
     *         parsed document
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        final Element originNode = TedTenderParserUtils.getOriginNode(document);

        final Element contractingBodyNode = TedTenderParserR209Utils.getSectionI(originNode);
        final Element contractNode = TedTenderParserR209Utils.getSectionII(originNode);
        final Element leftiNode = TedTenderParserR209Utils.getSectionIII(originNode);
        final Element procedureNode = TedTenderParserR209Utils.getSectionIV(originNode);
        final Element complementaryInfoNode = TedTenderParserR209Utils.getSectionVI(originNode);

        String procedureType = TedTenderParserR209Utils.parseProcedureType(procedureNode);

        parsedTender.setBuyers(TedTenderParserR209Utils.parseBuyers(document))
                .setIsJointProcurement(JsoupUtils.exists("JOINT_PROCUREMENT_INVOLVED", contractingBodyNode).toString())
                .setIsCentralProcurement(JsoupUtils.exists("CENTRAL_PURCHASING", contractingBodyNode).toString())
                .setDocuments(parseDocument(contractingBodyNode))
                .setFurtherInformationProvider(JsoupUtils.exists("ADDRESS_FURTHER_INFO_IDEM",
                        contractNode) ? null : TedTenderParserR209Utils.parseBody(
                        JsoupUtils.selectFirst("ADDRESS_FURTHER_INFO", contractingBodyNode)))
                .setBidsRecipient(JsoupUtils.exists("ADDRESS_PARTICIPATION_IDEM",
                        contractingBodyNode) ? null : TedTenderParserR209Utils.parseBody(
                        JsoupUtils.selectFirst("ADDRESS_PARTICIPATION", contractingBodyNode)))
                .setTitle(JsoupUtils.selectText("TITLE", contractNode))
                .setCpvs(TedTenderParserR209Utils.parseCpvs(JsoupUtils.selectFirst("CPV_MAIN", contractNode)))
                .setSupplyType(JsoupUtils.selectAttribute("TYPE_CONTRACT", "CTYPE", contractNode))
                .setDescription(JsoupUtils.selectText("SHORT_DESCR", contractNode))
                .setEstimatedPrice(TedTenderParserR209Utils.parsePrice(
                        JsoupUtils.selectFirst("VAL_ESTIMATED_TOTAL", contractNode)))
                .setHasLots(JsoupUtils.exists("LOT_DIVISION", contractNode).toString())
                .setLots(parseLots(JsoupUtils.select("OBJECT_DESCR", contractNode)))
                .setEconomicRequirements(
                        JsoupUtils.selectCombinedText("ECONOMIC_FINANCIAL_INFO, ECONOMIC_FINANCIAL_MIN_LEVEL",
                                leftiNode))
                .setPersonalRequirements(JsoupUtils.selectText("SUITABILITY", leftiNode))
                .setTechnicalRequirements(
                        JsoupUtils.selectCombinedText("TECHNICAL_PROFESSIONAL_INFO, TECHNICAL_PROFESSIONAL_MIN_LEVEL",
                                leftiNode))
                .setDeposits(JsoupUtils.selectText("DEPOSIT_GUARANTEE_REQUIRED", leftiNode))
                .setProcedureType(procedureType)
                .setNationalProcedureType(procedureType)
                .setIsFrameworkAgreement(JsoupUtils.exists("FRAMEWORK", procedureNode).toString())
                .setMaxFrameworkAgreementParticipants(
                        JsoupUtils.selectText("FRAMEWORK > NB_PARTICIPANTS", procedureNode))
                .setIsDps(JsoupUtils.exists("DPS", procedureNode).toString())
                .setIsElectronicAuction(JsoupUtils.exists("EAUCTION_USED", procedureNode).toString())
                .setIsCoveredByGpa(JsoupUtils.exists("CONTRACT_COVERED_GPA", procedureNode).toString())
                .setBidDeadline(JsoupUtils.selectText("DATE_RECEIPT_TENDERS", procedureNode,
                        true) + " " + JsoupUtils.selectText("TIME_RECEIPT_TENDERS", procedureNode, true))
                .setEligibleBidLanguages(TedTenderParserR209Utils.parseEligibleBidLanguages(
                        JsoupUtils.select("LANGUAGES > LANGUAGE", procedureNode)))
                .setIsEInvoiceAccepted(JsoupUtils.exists("EINVOICING", complementaryInfoNode).toString())
                .setAppealBodyName(JsoupUtils.selectText("ADDRESS_REVIEW_BODY", complementaryInfoNode))
                .setMediationBodyName(JsoupUtils.selectText("ADDRESS_MEDIATION_BODY", complementaryInfoNode));

        return parsedTender;
    }

    /**
     * Parses lots from node.
     *
     * @param lotsNodes
     *         node with lots data
     *
     * @return list of parsed lots or null if list is empty
     */
    private static List<ParsedTenderLot> parseLots(final Elements lotsNodes) {
        if (lotsNodes == null || lotsNodes.isEmpty()) {
            return null;
        }

        final List<ParsedTenderLot> lots = new ArrayList();

        int position = 1;
        for (final Element lotNode : lotsNodes) {
            lots.add(new ParsedTenderLot()
                .setPositionOnPage(String.valueOf(position++))
                .setLotNumber(JsoupUtils.selectText("LOT_NO", lotNode))
                .setTitle(JsoupUtils.selectText("TITLE", lotNode))
                .setDescription(JsoupUtils.selectText("SHORT_DESCR", lotNode))
                .setEstimatedPrice(
                    TedTenderParserR209Utils.parsePrice(JsoupUtils.selectFirst("VAL_OBJECT", lotNode)))
                .setCpvs(TedTenderParserR209Utils.parseCpvs(JsoupUtils.selectFirst("CPV_ADDITIONAL", lotNode)))
                .setAddressOfImplementation(TedTenderParserR209Utils.parseAddressOfImplementation(lotNode))
                .setAwardCriteria(TedTenderParserR209Utils.parseAwardCriteria(lotNode))
                .setEstimatedStartDate(JsoupUtils.selectText("DATE_START", lotNode))
                .setEstimatedCompletionDate(JsoupUtils.selectText("DATE_END", lotNode))
                .setEstimatedDurationInDays(JsoupUtils.selectText("DURATION[TYPE=DAY]", lotNode))
                .setEstimatedDurationInMonths(JsoupUtils.selectText("DURATION[TYPE=MONTH]", lotNode))
                .setMaxFrameworkAgreementParticipants(JsoupUtils.selectText("NB_ENVISAGED_CANDIDATE", lotNode))
                .addFunding(TedTenderParserR209Utils.parseFunding(lotNode))
                .setAreVariantsAccepted(JsoupUtils.exists("ACCEPTED_VARIANTS", lotNode).toString())
                .setHasOptions(JsoupUtils.exists("OPTIONS", lotNode).toString()));
        }

        return lots;
    }

    /**
     * Parses document.
     *
     * @param contractingBodyNode
     *         node with contracting body data
     *
     * @return list containing parsed document
     */
    private static List<ParsedDocument> parseDocument(final Element contractingBodyNode) {
        final String documentUrl = JsoupUtils.selectText("URL_DOCUMENT", contractingBodyNode);
        return (documentUrl == null ? null : Arrays.asList(new ParsedDocument().setUrl(documentUrl)));
    }
}
