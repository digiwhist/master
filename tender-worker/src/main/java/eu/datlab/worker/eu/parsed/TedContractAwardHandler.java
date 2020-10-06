package eu.datlab.worker.eu.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
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
 * Parser for TED award notice form.
 *
 * @author Tomas Mrazek
 */
public final class TedContractAwardHandler {

    /**
     * Source for TED publications.
     */
    private static final String PUBLICATIONS_SOURCE = PublicationSources.EU_TED;

    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedContractAwardHandler() {
        throw new AssertionError();
    }

    /**
     * Parses contract award form specific data.
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
        final Element contractingBodyNode =
            //CONTACTING_AUTHORITY_INFORMATION (maybe typo) probably occurs only in form of version R2.0.7.S03.E01
            JsoupUtils.selectFirst("CONTRACTING_AUTHORITY_INFORMATION_CONTRACT_AWARD, CONTACTING_AUTHORITY_INFORMATION,"
                + "CONTRACTING_AUTHORITY_INFORMATION_CONTRACT_AWARD_DEFENCE",
                originNode);
        //II
        final Element contractNode = JsoupUtils.selectFirst("OBJECT_CONTRACT_INFORMATION_CONTRACT_AWARD_NOTICE,"
            + "OBJECT_CONTRACT_INFORMATION_CONTRACT_AWARD_NOTICE_DEFENCE", originNode);
        //IV
        final Element procedureNode = JsoupUtils.selectFirst("PROCEDURE_DEFINITION_CONTRACT_AWARD_NOTICE,"
            + "PROCEDURE_DEFINITION_CONTRACT_AWARD_NOTICE_DEFENCE", originNode);
        //VI
        final Element complementaryInfoNode = JsoupUtils.selectFirst("COMPLEMENTARY_INFORMATION_CONTRACT_AWARD",
                originNode);

        final Element onBehalfOfNode = JsoupUtils.selectFirst(
            "TYPE_AND_ACTIVITIES_AND_PURCHASING_ON_BEHALF > PURCHASING_ON_BEHALF > PURCHASING_ON_BEHALF_YES,"
            + "TYPE_AND_ACTIVITIES_OR_CONTRACTING_ENTITY_AND_PURCHASING_ON_BEHALF > PURCHASING_ON_BEHALF > PURCHASING_ON_BEHALF_YES",
                contractingBodyNode);

        final Element awardCriteriaNode = JsoupUtils.selectFirst(
            "AWARD_CRITERIA_CONTRACT_AWARD_NOTICE_INFORMATION > AWARD_CRITERIA_DETAIL_F03,"
            + "AWARD_CRITERIA_CONTRACT_AWARD_NOTICE_INFORMATION_DEFENCE > AWARD_CRITERIA_DETAIL_F18", procedureNode);

        String procedureType = TedTenderParserUtils.parseProcedureType(
                JsoupUtils.selectFirst("TYPE_OF_PROCEDURE_DEF, TYPE_OF_PROCEDURE_CONTRACT_AWARD_DEFENCE", procedureNode));

        parsedTender
            .addPublication(parseMainPublication(originNode))
            .addPublications(parseOtherPublications(JsoupUtils.select(
                "PREVIOUS_PUBLICATION_NOTICE_F3, CNT_NOTICE_INFORMATION, EX_ANTE_NOTICE_INFORMATION,"
                    + " OTHER_PREVIOUS_PUBLICATIONS > OTHER_PREVIOUS_PUBLICATION, CNT_NOTICE_INFORMATION_F18",
                JsoupUtils.selectFirst(
                    "ADMINISTRATIVE_INFORMATION_CONTRACT_AWARD > PREVIOUS_PUBLICATION_INFORMATION_NOTICE_F3"
                        + " > PREVIOUS_PUBLICATION_EXISTS_F3, ADMINISTRATIVE_INFORMATION_CONTRACT_AWARD_DEFENCE"
                        + " > PREVIOUS_PUBLICATION_INFORMATION_NOTICE_F18 > PREVIOUS_PUBLICATION_EXISTS_F18", procedureNode))))
            .addBuyer(TedTenderParserUtils.parseBuyer(contractingBodyNode))
            .setOnBehalfOf(TedTenderParserUtils.parseBodies(
                JsoupUtils.select("CONTACT_DATA_OTHER_BEHALF_CONTRACTING_AUTORITHY", onBehalfOfNode)))
            .setIsOnBehalfOf(onBehalfOfNode != null ? Boolean.TRUE.toString() : Boolean.FALSE.toString())
            .setFurtherInformationProvider(JsoupUtils.exists("FURTHER_INFORMATION > IDEM",
                contractingBodyNode) ? null : TedTenderParserUtils.parseBody(
                    JsoupUtils.selectFirst("FURTHER_INFORMATION", contractingBodyNode)))
            .setSpecificationsProvider(JsoupUtils.exists("SPECIFICATIONS_AND_ADDITIONAL_DOCUMENTS > IDEM",
                contractingBodyNode) ? null : TedTenderParserUtils.parseBody(
                    JsoupUtils.selectFirst("SPECIFICATIONS_AND_ADDITIONAL_DOCUMENTS", contractingBodyNode)))
            .addAdministrator(JsoupUtils.exists("TENDERS_REQUESTS_APPLICATIONS_MUST_BE_SENT_TO > IDEM",
                contractingBodyNode) ? null : TedTenderParserUtils.parseBody(
                    JsoupUtils.selectFirst("TENDERS_REQUESTS_APPLICATIONS_MUST_BE_SENT_TO", contractingBodyNode)))
            .setDescription(JsoupUtils.selectText("DESCRIPTION_AWARD_NOTICE_INFORMATION > SHORT_CONTRACT_DESCRIPTION,"
                + "DESCRIPTION_AWARD_NOTICE_INFORMATION_DEFENCE > SHORT_CONTRACT_DESCRIPTION", contractNode))
            .setProcedureType(procedureType)
            .setNationalProcedureType(procedureType)
            .setSupplyType(JsoupUtils.selectAttribute("TYPE_CONTRACT_LOCATION_W_PUB > TYPE_CONTRACT", "VALUE", contractNode))
            .setAddressOfImplementation(TedTenderParserUtils.parseAddressOfImplementation(contractNode))
            .setIsFrameworkAgreement(JsoupUtils.exists("NOTICE_INVOLVES_DESC > CONCLUSION_FRAMEWORK_AGREEMENT", contractNode).toString())
            .setIsDps(JsoupUtils.exists("NOTICE_INVOLVES_DESC > CONTRACTS_DPS", contractNode).toString())
            .setCpvs(TedTenderParserUtils.parseCpvs(JsoupUtils.selectFirst("CPV", contractNode)))
            .setIsCoveredByGpa(JsoupUtils.hasAttribute(JsoupUtils.selectFirst("CONTRACT_COVERED_GPA", contractNode), "VALUE", "YES")
                .toString())
            .setFinalPrice(TedTenderParserUtils.parsePrice(JsoupUtils.selectFirst("TOTAL_FINAL_VALUE"
                + " > COSTS_RANGE_AND_CURRENCY_WITH_VAT_RATE, COSTS_RANGE_AND_CURRENCY_WITH_VAT_RATE", contractNode)))
            .setLots(parseLots(JsoupUtils.select("AWARD_OF_CONTRACT, AWARD_OF_CONTRACT_DEFENCE", originNode)))
            .setAwardCriteria(TedTenderParserUtils.parseAwardCritera(awardCriteriaNode))
            .setSelectionMethod(TedTenderParserUtils.parseSelectionMethod(codedDataNode))
            .setIsElectronicAuction(JsoupUtils.hasAttribute(
                JsoupUtils.selectFirst("AWARD_CRITERIA_CONTRACT_AWARD_NOTICE_INFORMATION > F03_IS_ELECTRONIC_AUCTION_USABLE,"
                    + "AWARD_CRITERIA_CONTRACT_AWARD_NOTICE_INFORMATION_DEFENCE > F18_IS_ELECTRONIC_AUCTION_USABLE",
                    procedureNode), "VALUE", "YES").toString())
            .addFunding(TedTenderParserUtils.parseFunding(complementaryInfoNode))
            .setAppealBodyName(JsoupUtils.selectText(
                "PROCEDURES_FOR_APPEAL > APPEAL_PROCEDURE_BODY_RESPONSIBLE > CONTACT_DATA_WITHOUT_RESPONSIBLE_NAME"
                    + " > ORGANISATION > OFFICIALNAME", complementaryInfoNode))
            .setMediationBodyName(JsoupUtils.selectText(
                "PROCEDURES_FOR_APPEAL > MEDIATION_PROCEDURE_BODY_RESPONSIBLE > CONTACT_DATA_WITHOUT_RESPONSIBLE_NAME"
                    + " > ORGANISATION > OFFICIALNAME", complementaryInfoNode))
            .setNpwpReasons(TedTenderParserUtils.parseNpwpReasons(JsoupUtils.selectFirst("TYPE_OF_PROCEDURE_DEF"
                + " > F03_PT_NEGOTIATED_WITHOUT_COMPETITION", procedureNode)));

        TedTenderParserUtils.appendNoticeReference(document, parsedTender);

        return parsedTender;
    }

    /**
     * Parses lots.
     *
     * @param lotsNodes
     *         lots nodes
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
                .setContractNumber(JsoupUtils.selectText("CONTRACT_NUMBER", lotNode))
                .setLotNumber(JsoupUtils.selectText("LOT_NUMBER", lotNode))
                .setTitle(JsoupUtils.selectText("CONTRACT_TITLE", lotNode))
                .setAwardDecisionDate(
                    TedTenderParserUtils.parseDateTime(JsoupUtils.selectFirst("CONTRACT_AWARD_DATE", lotNode)))
                .setBidsCount(JsoupUtils.selectText("OFFERS_RECEIVED_NUMBER", lotNode))
                .setElectronicBidsCount(JsoupUtils.selectText("OFFERS_RECEIVED_NUMBER_MEANING", lotNode))
                .addBid(parseWinningBid(lotNode))
                .setEstimatedPrice(TedTenderParserUtils.parsePrice(JsoupUtils.selectFirst(
                    "INITIAL_ESTIMATED_TOTAL_VALUE_CONTRACT", lotNode))));
        }

        return lots;
    }

    /**
     * Parses winnig bid for given lot. For TED specifically, the document contains only winnig bidder/supplier and his
     * bid.
     *
     * @param lotNode
     *         node with lot data
     *
     * @return winning bid
     */
    private static ParsedBid parseWinningBid(final Element lotNode) {
        if (lotNode == null) {
            return null;
        }

        final Element sectionV4 = JsoupUtils.selectFirst("CONTRACT_VALUE_INFORMATION", lotNode);
        final Element sectionV5 = JsoupUtils.selectFirst("MORE_INFORMATION_TO_SUB_CONTRACTED"
            + " > CONTRACT_LIKELY_SUB_CONTRACTED", lotNode);

        return new ParsedBid()
            .addBidder(TedTenderParserUtils.parseBody(lotNode))
            .setIsWinning(Boolean.TRUE.toString())
            .setAnnualPriceYearsCount(JsoupUtils.selectText("NUMBER_OF_YEARS", lotNode))
            .setMonthlyPriceMonthsCount(JsoupUtils.selectText("NUMBER_OF_MONTHS", lotNode))
            .setPrice(TedTenderParserUtils.parsePrice(
                JsoupUtils.selectFirst("COSTS_RANGE_AND_CURRENCY_WITH_VAT_RATE", sectionV4)))
            .setIsSubcontracted(Boolean.toString(sectionV5 != null))
            .setSubcontractedProportion(JsoupUtils.selectText("EXCLUDING_VAT_PRCT", sectionV5))
            .setSubcontractedValue(new ParsedPrice()
                .setNetAmount(JsoupUtils.selectText("EXCLUDING_VAT_VALUE", sectionV5))
                .setCurrency(JsoupUtils.selectAttribute("EXCLUDING_VAT_VALUE", "CURRENCY", sectionV5)));
    }

    /**
     * Parses main tender publication.
     *
     * @param originNode
     *         origin node for document parsing
     *
     * @return parsed main publication
     */
    private static ParsedPublication parseMainPublication(final Element originNode) {
        return TedTenderParserUtils.initMainPublication(originNode.ownerDocument())
            .setDispatchDate(TedTenderParserUtils.parseDateTime(JsoupUtils.selectFirst("NOTICE_DISPATCH_DATE",
                originNode)))
            .setBuyerAssignedId(JsoupUtils.selectText("PROCEDURE_DEFINITION_CONTRACT_AWARD_NOTICE"
                + " > ADMINISTRATIVE_INFORMATION_CONTRACT_AWARD > FILE_REFERENCE_NUMBER,"
                + "PROCEDURE_DEFINITION_CONTRACT_AWARD_NOTICE_DEFENCE > ADMINISTRATIVE_INFORMATION_CONTRACT_AWARD_DEFENCE"
                + " > FILE_REFERENCE_NUMBER", originNode))
            .setSource(PUBLICATIONS_SOURCE);
    }

    /**
     * Parses other publications.
     *
     * @param publicationNodes
     *         publication nodes
     *
     * @return list of parsed publications
     */
    private static List<ParsedPublication> parseOtherPublications(final Elements publicationNodes) {
        if (publicationNodes == null || publicationNodes.isEmpty()) {
            return null;
        }

        final List<ParsedPublication> publications = new ArrayList<>();
        for (Element node : publicationNodes) {
            String sourceFormType = null;
            if (node.tagName().equalsIgnoreCase("OTHER_PREVIOUS_PUBLICATION")) {
                sourceFormType = node.tagName();
            } else if (node.tagName().matches("PREVIOUS_PUBLICATION_NOTICE_F3|CNT_NOTICE_INFORMATION|CNT_NOTICE_INFORMATION_F18|"
                + "PREVIOUS_PUBLICATION_NOTICE_F18")) {
                sourceFormType = JsoupUtils.selectAttribute("*[CHOICE]", "CHOICE", node);
            } else if (node.tagName().equalsIgnoreCase("EX_ANTE_NOTICE_INFORMATION")) {
                sourceFormType = JsoupUtils.selectAttribute("*[VALUE]", "VALUE", node);
            }

            publications.add(new ParsedPublication().setSourceFormType(sourceFormType)
                .setSourceId(JsoupUtils.selectText("NOTICE_NUMBER_OJ", node))
                .setPublicationDate(TedTenderParserUtils.parseDateTime(JsoupUtils.selectFirst("DATE_OJ", node)))
                .setIsIncluded(false)
                .setSource(PUBLICATIONS_SOURCE));
        }

        return publications;
    }
}
