package eu.digiwhist.worker.eu.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import java.util.ArrayList;
import java.util.List;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

/**
 * Parser for TED award notice utilities form.
 *
 * @author Tomas Mrazek
 */
public final class TedContractAwardUtilitiesHandler {
    
    /**
     * Source for TED publications.
     */
    private static final String PUBLICATIONS_SOURCE = PublicationSources.EU_TED;
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedContractAwardUtilitiesHandler() {
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
        final Element contractingBodyNode = JsoupUtils.selectFirst("CONTRACTING_ENTITY_CONTRACT_AWARD_UTILITIES",
                originNode);
        //II
        final Element contractNode = JsoupUtils.selectFirst("OBJECT_CONTRACT_AWARD_UTILITIES", originNode);
        //IV
        final Element procedureNode = JsoupUtils.selectFirst("PROCEDURES_CONTRACT_AWARD_UTILITIES", originNode);
        //VI
        final Element complementaryInfoNode = JsoupUtils.selectFirst(
                "COMPLEMENTARY_INFORMATION_CONTRACT_AWARD_UTILITIES", originNode);

        final Element onBehalfOfNode =
            JsoupUtils.selectFirst("PURCHASING_ON_BEHALF > PURCHASING_ON_BEHALF_YES", contractingBodyNode);

        final Element awardCriterionNode =
            JsoupUtils.selectFirst("F06_AWARD_CRITERIA_CONTRACT_UTILITIES_INFORMATION > PRICE_AWARD_CRITERIA",
                procedureNode);

        String procedureType = TedTenderParserUtils.parseProcedureType(
                JsoupUtils.selectFirst("TYPE_PROCEDURE_AWARD", procedureNode));

        parsedTender
            .addPublication(parseMainPublication(originNode))
            .addPublications(parseOtherPublications(
                JsoupUtils.select("PREVIOUS_PUBLICATION_NOTICE_F6, CNT_NOTICE_INFORMATION, EX_ANTE_NOTICE_INFORMATION,"
                    + " OTHER_PREVIOUS_PUBLICATIONS > OTHER_PREVIOUS_PUBLICATION",
                JsoupUtils.selectFirst(
                    "ADMINISTRATIVE_INFO_CONTRACT_AWARD_UTILITIES > PREVIOUS_PUBLICATION_INFORMATION_NOTICE_F6"
                        + " > PREVIOUS_PUBLICATION_EXISTS_F6", procedureNode))))
            .addBuyer(parseBuyer(contractingBodyNode))
            .setOnBehalfOf(TedTenderParserUtils.parseBodies(
                JsoupUtils.select("CONTACT_DATA_OTHER_BEHALF_CONTRACTING_AUTORITHY", onBehalfOfNode)))
            .setIsOnBehalfOf(onBehalfOfNode != null ? Boolean.TRUE.toString() : Boolean.FALSE.toString())        
            .setDescription(
                JsoupUtils.selectText("DESCRIPTION_CONTRACT_AWARD_UTILITIES > SHORT_DESCRIPTION", contractNode))
            .setFurtherInformationProvider(JsoupUtils.exists("FURTHER_INFORMATION > IDEM",
                contractingBodyNode) ? null : TedTenderParserUtils.parseBody(
                    JsoupUtils.selectFirst("FURTHER_INFORMATION", contractingBodyNode)))
            .setSpecificationsProvider(JsoupUtils.exists("SPECIFICATIONS_AND_ADDITIONAL_DOCUMENTS > IDEM",
                contractingBodyNode) ? null : TedTenderParserUtils.parseBody(
                    JsoupUtils.selectFirst("SPECIFICATIONS_AND_ADDITIONAL_DOCUMENTS", contractingBodyNode)))
            .addAdministrator(JsoupUtils.exists("TENDERS_REQUESTS_APPLICATIONS_MUST_BE_SENT_TO > IDEM",
                contractingBodyNode) ? null : TedTenderParserUtils.parseBody(
                    JsoupUtils.selectFirst("TENDERS_REQUESTS_APPLICATIONS_MUST_BE_SENT_TO", contractingBodyNode)))
            .setProcedureType(procedureType)
            .setNationalProcedureType(procedureType)
            .setSupplyType(
                JsoupUtils.selectAttribute("TYPE_CONTRACT_LOCATION_W_PUB > TYPE_CONTRACT ", "VALUE", contractNode))
            .setAddressOfImplementation(TedTenderParserUtils.parseAddressOfImplementation(contractNode))
            .setIsFrameworkAgreement(
                    JsoupUtils.exists("F06_NOTICE_INVOLVES > CONCLUSION_FRAMEWORK_AGREEMENT", contractNode)
                            .toString())
            .setIsDps(JsoupUtils.exists("F06_NOTICE_INVOLVES > CONTRACTS_DPS", contractNode).toString())
            .setCpvs(TedTenderParserUtils.parseCpvs(JsoupUtils.selectFirst("CPV", contractNode)))
            .setIsCoveredByGpa(
                JsoupUtils.hasAttribute(JsoupUtils.selectFirst("CONTRACT_COVERED_GPA", contractNode), "VALUE", "YES")
                    .toString())
            .setFinalPrice(TedTenderParserUtils.parsePrice(
                JsoupUtils.selectFirst("TOTAL_FINAL_VALUE > COSTS_RANGE_AND_CURRENCY_WITH_VAT_RATE,"
                    + " COSTS_RANGE_AND_CURRENCY_WITH_VAT_RATE", contractNode)))
            .setLots(parseLots(JsoupUtils.select("AWARD_OF_CONTRACT, AWARD_AND_CONTRACT_VALUE", originNode)))
            .addAwardCriterion(parseAwardCritera(awardCriterionNode))
            .setSelectionMethod(TedTenderParserUtils.parseSelectionMethod(codedDataNode))
            .setIsElectronicAuction(
                JsoupUtils.exists("F06_AWARD_CRITERIA_CONTRACT_UTILITIES_INFORMATION > F06_ELECTRONIC_AUCTION",
                    procedureNode).toString())
            .addFunding(TedTenderParserUtils.parseFunding(complementaryInfoNode))
            .setAppealBodyName(JsoupUtils.selectText(
                "APPEAL_PROCEDURES > RESPONSIBLE_FOR_APPEAL_PROCEDURES > CONTACT_DATA_WITHOUT_RESPONSIBLE_NAME"
                    + " > ORGANISATION > OFFICIALNAME", complementaryInfoNode))
            .setMediationBodyName(JsoupUtils.selectText(
                "APPEAL_PROCEDURES > MEDIATION_PROCEDURE_BODY_RESPONSIBLE > CONTACT_DATA_WITHOUT_RESPONSIBLE_NAME"
                    + " > ORGANISATION > OFFICIALNAME", complementaryInfoNode))
            .setNpwpReasons(TedTenderParserUtils.parseNpwpReasons(JsoupUtils.selectFirst("TYPE_PROCEDURE_AWARD"
                + " > F06_PT_NEGOTIATED_WITHOUT_COMPETITION", procedureNode)));

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
                .setContractNumber(JsoupUtils.selectText("CONTRACT_NUMBER, CONTRACT_NO", lotNode))
                .setLotNumber(JsoupUtils.selectText("LOT_NUMBER", lotNode))
                .setTitle(JsoupUtils.selectText("CONTRACT_TITLE, TITLE_CONTRACT", lotNode))
                .setAwardDecisionDate(
                    TedTenderParserUtils.parseDateTime(
                        JsoupUtils.selectFirst("CONTRACT_AWARD_DATE, DATE_OF_CONTRACT_AWARD", lotNode)))
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

        final Element sectionV4 = JsoupUtils.selectFirst("INFORMATION_VALUE_CONTRACT", lotNode);
        final Element sectionV5 = JsoupUtils.selectFirst("CONTRACT_LIKELY_SUB_CONTRACTED", lotNode);

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
     * @return parsed main publication
     */
    private static ParsedPublication parseMainPublication(final Element originNode) {
        return TedTenderParserUtils.initMainPublication(originNode.ownerDocument())
            .setDispatchDate(
                TedTenderParserUtils.parseDateTime(JsoupUtils.selectFirst("NOTICE_DISPATCH_DATE", originNode)))
            .setBuyerAssignedId(JsoupUtils.selectText(
                "PROCEDURES_CONTRACT_AWARD_UTILITIES > ADMINISTRATIVE_INFO_CONTRACT_AWARD_UTILITIES"
                    + " > REFERENCE_NUMBER_ATTRIBUTED", originNode))            
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
            } else if (node.tagName().matches("PREVIOUS_PUBLICATION_NOTICE_F6|CNT_NOTICE_INFORMATION")) {
                sourceFormType = JsoupUtils.selectAttribute("*[CHOICE]", "CHOICE", node);
            } else if (node.tagName().equals("EX_ANTE_NOTICE_INFORMATION")) {
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

    /**
     * Parses buyer.
     *
     * @param buyerNode
     *         node with buyer data
     *
     * @return parsed buyer
     */
    private static ParsedBody parseBuyer(final Element buyerNode) {
        if (buyerNode == null) {
            return null;
        }

        return TedTenderParserUtils.parseBody(
            JsoupUtils.selectFirst("NAME_ADDRESSES_CONTACT_CONTRACT_AWARD_UTILITIES", buyerNode))
            .setBuyerType(TedTenderParserUtils.getDefaultBuyerType(buyerNode.ownerDocument()))
            .setMainActivities(TedTenderParserUtils.parseBuyerMainActivities(JsoupUtils.select(
                "ACTIVITY_OF_CONTRACTING_ENTITY, ACTIVITY_OF_CONTRACTING_ENTITY_OTHER", buyerNode),
                buyerNode.ownerDocument()));
    }

    /**
     * Parses award criteria.
     *
     * @param awardCriterionNode
     *         award criterion node
     *
     * @return parsed award criterion
     */
    private static ParsedAwardCriterion parseAwardCritera(final Element awardCriterionNode) {
        final String criterion = JsoupUtils.selectAttribute("PRICE", awardCriterionNode);

        if (criterion == null) {
            return null;
        }

        return new ParsedAwardCriterion().setName(criterion);
    }
}
