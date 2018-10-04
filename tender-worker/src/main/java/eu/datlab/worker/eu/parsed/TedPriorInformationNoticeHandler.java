package eu.datlab.worker.eu.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.List;

/**
 * Parser for TED prior information notice form.
 *
 * @author Tomas Mrazek
 */
public final class TedPriorInformationNoticeHandler {

    /**
     * Source for TED publications.
     */
    private static final String PUBLICATIONS_SOURCE = PublicationSources.EU_TED;
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedPriorInformationNoticeHandler() {
        throw new AssertionError();
    }

    /**
     * Parses contract notice form specific data.
     *
     * @param parsedTender
     *         initialized parsedTender
     * @param document
     *         parsed document
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        final Element originNode = TedTenderParserUtils.getOriginNode(document);
        final Element codedDataNode = TedTenderParserUtils.getCodedDataNode(document);

        //I
        final Element contractingBodyNode = JsoupUtils.selectFirst("AUTHORITY_PRIOR_INFORMATION", originNode);
        //II
        final Element contractNode = JsoupUtils.selectFirst("OBJECT_WORKS_PRIOR_INFORMATION,"
            + " OBJECT_SUPPLIES_SERVICES_PRIOR_INFORMATION", originNode);
        
        final Element onBehalfOfNode = JsoupUtils.selectFirst(
            "TYPE_AND_ACTIVITIES_AND_PURCHASING_ON_BEHALF > PURCHASING_ON_BEHALF > PURCHASING_ON_BEHALF_YES",
                contractingBodyNode);

        parsedTender
            .addPublication(parseMainPublication(originNode))
            .addBuyer(TedTenderParserUtils.parseBuyer(contractingBodyNode))
            .setOnBehalfOf(TedTenderParserUtils.parseBodies(
                JsoupUtils.select("CONTACT_DATA_OTHER_BEHALF_CONTRACTING_AUTORITHY", onBehalfOfNode)))
            .setIsOnBehalfOf(onBehalfOfNode != null ? Boolean.TRUE.toString() : Boolean.FALSE.toString())
            .setIsCoveredByGpa(
                JsoupUtils.hasAttribute(JsoupUtils.selectFirst("CONTRACT_COVERED_GPA", contractNode), "VALUE", "YES")
                    .toString())
            .setHasLots(
                Boolean.toString(!JsoupUtils.exists("F01_DIVISION_INTO_LOTS > DIV_INTO_LOT_NO", contractNode)))
            .setLots(parseLots(
                JsoupUtils.select("F01_DIVISION_INTO_LOTS > F01_DIV_INTO_LOT_YES > F01_ANNEX_B", contractNode)))
            .setIsFrameworkAgreement(JsoupUtils.hasAttribute(JsoupUtils.selectFirst("FRAMEWORK_AGREEMENT",
                contractNode), "VALUE", "YES").toString())
            .setEstimatedPrice(TedTenderParserUtils.parsePrice(
                JsoupUtils.selectFirst("QUANTITY_SCOPE_PRIOR_INFORMATION > COSTS_RANGE_AND_CURRENCY",
                    contractNode)))
            .setFurtherInformationProvider(JsoupUtils.exists("FURTHER_INFORMATION > IDEM",
                contractingBodyNode) ? null : TedTenderParserUtils.parseBody(
                    JsoupUtils.selectFirst("FURTHER_INFORMATION", contractingBodyNode)))
            .setSpecificationsProvider(JsoupUtils.exists("SPECIFICATIONS_AND_ADDITIONAL_DOCUMENTS > IDEM",
                contractingBodyNode) ? null : TedTenderParserUtils.parseBody(
                    JsoupUtils.selectFirst("SPECIFICATIONS_AND_ADDITIONAL_DOCUMENTS", contractingBodyNode)))
            .addAdministrator(JsoupUtils.exists("TENDERS_REQUESTS_APPLICATIONS_MUST_BE_SENT_TO > IDEM",
                contractingBodyNode) ? null : TedTenderParserUtils.parseBody(
                    JsoupUtils.selectFirst("TENDERS_REQUESTS_APPLICATIONS_MUST_BE_SENT_TO", contractingBodyNode)))
            .setCpvs(TedTenderParserUtils.parseCpvs(JsoupUtils.selectFirst("CPV", contractNode)))
            .setDescription(
                JsoupUtils.selectText("DESCRIPTION_CONTRACT_INFORMATION > SHORT_CONTRACT_DESCRIPTION", contractNode))
            .setSupplyType(
                JsoupUtils.selectAttribute("TYPE_CONTRACT_LOCATION > TYPE_CONTRACT ", "VALUE", contractNode))
            .setEstimatedStartDate(TedTenderParserUtils.parseDateTime(
                JsoupUtils.selectFirst("PERIOD_WORK_DATE_STARTING > INTERVAL_DATE > START_DATE", contractNode)))
            .setEstimatedCompletionDate(TedTenderParserUtils.parseDateTime(
                JsoupUtils.selectFirst("PERIOD_WORK_DATE_STARTING > INTERVAL_DATE > END_DATE", contractNode)))
            .setAddressOfImplementation(parseAddressOfImplementation(contractNode))
            .setIsFrameworkAgreement(JsoupUtils.hasAttribute(JsoupUtils.selectFirst("FRAMEWORK_AGREEMENT",
                contractNode), "VALUE", "YES").toString())
            .setAreVariantsAccepted(
                JsoupUtils.hasAttribute(JsoupUtils.selectFirst("ACCEPTED_VARIANTS", contractNode), "VALUE", "YES")
                    .toString())
            .setHasOptions(Boolean.toString(!JsoupUtils.exists("NO_OPTIONS", contractNode)))            
            .setSelectionMethod(TedTenderParserUtils.parseSelectionMethod(codedDataNode));

        TedTenderParserUtils.appendNoticeReference(document, parsedTender);

        return parsedTender;
    }

    /**
     * @param contractNode
     *      node that includes contract data
     * @return address of implementation
     */
    private static ParsedAddress parseAddressOfImplementation(final Element contractNode) {
        Element addrNode = JsoupUtils.selectFirst("SITE_OR_LOCATION", contractNode);
        
        return new ParsedAddress()
            .addNuts(JsoupUtils.selectAttribute("CODE", JsoupUtils.selectFirst("*[CODE]", addrNode)))
            .setRawAddress(JsoupUtils.selectText("LABEL", addrNode));
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
                .setLotNumber(JsoupUtils.selectText("LOT_NUMBER", lotNode))
                .setTitle(JsoupUtils.selectText("LOT_TITLE", lotNode))
                .setDescription(JsoupUtils.selectText("LOT_DESCRIPTION", lotNode))
                .setCpvs(TedTenderParserUtils.parseCpvs(JsoupUtils.selectFirst("CPV", lotNode)))
                .setEstimatedPrice(TedTenderParserUtils.parsePrice(
                    JsoupUtils.selectFirst("NATURE_QUANTITY_SCOPE > COSTS_RANGE_AND_CURRENCY", lotNode)))
                .setEstimatedStartDate(TedTenderParserUtils.parseDateTime(
                    JsoupUtils.selectFirst("PERIOD_WORK_DATE_STARTING > INTERVAL_DATE > START_DATE", lotNode)))
                .setEstimatedCompletionDate(TedTenderParserUtils.parseDateTime(
                    JsoupUtils.selectFirst("PERIOD_WORK_DATE_STARTING > INTERVAL_DATE > END_DATE", lotNode))));
        }

        return lots;
    }


    /**
     * Parses main tender publication.
     *
     * @param originNode
     *         origin node for document parsing
     * @return parsed main publication
     */
    private static ParsedPublication parseMainPublication(final Element originNode) {
        return TedTenderParserUtils.initMainPublication(originNode.ownerDocument())
            .setDispatchDate(
                TedTenderParserUtils.parseDateTime(JsoupUtils.selectFirst("NOTICE_DISPATCH_DATE", originNode)))
            .setSource(PUBLICATIONS_SOURCE);
    }
}
