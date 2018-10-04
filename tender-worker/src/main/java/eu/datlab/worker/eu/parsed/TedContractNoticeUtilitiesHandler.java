package eu.datlab.worker.eu.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
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
 * Parser for TED notice utilities form.
 *
 * @author Tomas Mrazek
 */
public final class TedContractNoticeUtilitiesHandler {

    /**
     * Source for TED publications.
     */
    private static final String PUBLICATIONS_SOURCE = PublicationSources.EU_TED;
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedContractNoticeUtilitiesHandler() {
        throw new AssertionError();
    }

    /**
     * Parses contract notice utilities form specific data.
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
            //CONTACTING_AUTHORITY_INFO (maybe typo) probably occurs only in form of version R2.0.7.S03.E01
            JsoupUtils.selectFirst("CONTRACTING_AUTHORITY_INFO, CONTACTING_AUTHORITY_INFO", originNode);
        //II
        final Element contractNode = JsoupUtils.selectFirst("OBJECT_CONTRACT_INFORMATION_CONTRACT_UTILITIES",
                originNode);
        //III
        final Element leftiNode = JsoupUtils.selectFirst("LEFTI_CONTRACT_NOTICE_UTILITIES", originNode);
        //IV
        final Element procedureNode = JsoupUtils.selectFirst("PROCEDURE_DEFINITION_CONTRACT_NOTICE_UTILITIES",
                originNode);
        //VI
        final Element complementaryInfoNode = JsoupUtils.selectFirst("COMPLEMENTARY_INFORMATION_CONTRACT_UTILITIES",
                originNode);

        final Element onBehalfOfNode =
            JsoupUtils.selectFirst("PURCHASING_ON_BEHALF > PURCHASING_ON_BEHALF_YES", contractingBodyNode);

        final Element awardCriteriaNode = JsoupUtils.selectFirst(
            "F05_AWARD_CRITERIA_CONTRACT_UTILITIES_INFORMATION > AWARD_CRITERIA_DETAIL", procedureNode);

        String procedureType = TedTenderParserUtils.parseProcedureType(
            JsoupUtils.selectFirst("TYPE_OF_PROCEDURE_FOR_CONTRACT", procedureNode));

        parsedTender
            .addPublication(parseMainPublication(originNode))
            .addPublications(parseOtherPublications(JsoupUtils.select(
                "PREVIOUS_PUBLICATION_NOTICE_F5, OTHER_PREVIOUS_PUBLICATIONS > OTHER_PREVIOUS_PUBLICATION",
                JsoupUtils.selectFirst("ADMINISTRATIVE_INFORMATION_CONTRACT_UTILITIES"
                    + " > PREVIOUS_PUBLICATION_INFORMATION_NOTICE_F5 > PREVIOUS_PUBLICATION_EXISTS_F5",
                    procedureNode))))
            .addBuyer(parseBuyer(contractingBodyNode))
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
            .setDescription(
                JsoupUtils.selectText("CONTRACT_OBJECT_DESCRIPTION > SHORT_CONTRACT_DESCRIPTION", contractNode))
            .setProcedureType(procedureType)
            .setNationalProcedureType(procedureType)
            .setSupplyType(
                JsoupUtils.selectAttribute("TYPE_CONTRACT_LOCATION > TYPE_CONTRACT ", "VALUE", contractNode))
            .setAddressOfImplementation(TedTenderParserUtils.parseAddressOfImplementation(contractNode))
            .setIsFrameworkAgreement(
                JsoupUtils.exists("NOTICE_INVOLVES > ESTABLISHMENT_FRAMEWORK_AGREEMENT", contractNode).toString())
            .setIsDps(JsoupUtils.exists("NOTICE_INVOLVES > SETTING_UP_DPS", contractNode).toString())
            .setMaxFrameworkAgreementParticipants(
                JsoupUtils.selectText("F05_FRAMEWORK > SEVERAL_OPERATORS > MAX_NUMBER_PARTICIPANTS", contractNode))
            .setEstimatedDurationInDays(
                JsoupUtils.selectText("OPTIONS > PROVISIONAL_TIMETABLE_DAYS, PERIOD_WORK_DATE_STARTING > DAYS",
                    contractNode))
            .setEstimatedDurationInMonths(JsoupUtils.selectText(
                "F05_FRAMEWORK DURATION_FRAMEWORK_MONTH, OPTIONS > PROVISIONAL_TIMETABLE_MONTHS,"
                    + " PERIOD_WORK_DATE_STARTING > MONTHS", contractNode))
            .setEstimatedPrice(TedTenderParserUtils.parsePrice(
                JsoupUtils.selectFirst("QUANTITY_SCOPE > NATURE_QUANTITY_SCOPE > COSTS_RANGE_AND_CURRENCY",
                    contractNode)))
            .setCpvs(TedTenderParserUtils.parseCpvs(JsoupUtils.selectFirst("CPV", contractNode)))
            .setIsCoveredByGpa(
                JsoupUtils.hasAttribute(JsoupUtils.selectFirst("CONTRACT_COVERED_GPA", contractNode), "VALUE", "YES")
                    .toString())
            .setHasLots(
                Boolean.toString(!JsoupUtils.exists("F05_DIVISION_INTO_LOTS > DIV_INTO_LOT_NO", contractNode)))
            .setLots(parseLots(
                JsoupUtils.select("F05_DIVISION_INTO_LOTS > F05_DIV_INTO_LOT_YES > F05_ANNEX_B", contractNode)))
            .setAreVariantsAccepted(
                JsoupUtils.hasAttribute(JsoupUtils.selectFirst("ACCEPTED_VARIANTS", contractNode), "VALUE", "YES")
                    .toString())
            .setHasOptions(Boolean.toString(!JsoupUtils.exists("NO_OPTIONS", contractNode)))
            .setEstimatedStartDate(TedTenderParserUtils.parseDateTime(
                JsoupUtils.selectFirst("PERIOD_WORK_DATE_STARTING > INTERVAL_DATE > START_DATE", contractNode)))
            .setEstimatedCompletionDate(TedTenderParserUtils.parseDateTime(
                JsoupUtils.selectFirst("PERIOD_WORK_DATE_STARTING > INTERVAL_DATE > END_DATE", contractNode)))
            .setDeposits(
                JsoupUtils.selectText("CONTRACT_RELATING_CONDITIONS > DEPOSITS_GUARANTEES_REQUIRED", leftiNode))
            .setPersonalRequirements(JsoupUtils.selectText(
                "F05_CONDITIONS_FOR_PARTICIPATION > ECONOMIC_OPERATORS_PERSONAL_SITUATION", leftiNode))
            .setEconomicRequirements(
                JsoupUtils.selectCombinedText("F05_ECONOMIC_FINANCIAL_CAPACITY > *", leftiNode))
            .setTechnicalRequirements(JsoupUtils.selectCombinedText("TECHNICAL_CAPACITY > *", leftiNode))
            .setAwardCriteria(TedTenderParserUtils.parseAwardCritera(awardCriteriaNode))
            .setSelectionMethod(TedTenderParserUtils.parseSelectionMethod(codedDataNode))
            .setIsElectronicAuction(JsoupUtils.exists(
                "F05_AWARD_CRITERIA_CONTRACT_UTILITIES_INFORMATION > IS_ELECTRONIC_AUCTION_USABLE"
                    + " > USE_ELECTRONIC_AUCTION", procedureNode).toString())
            .setDocumentsDeadline(TedTenderParserUtils.parseDateTime(JsoupUtils.selectFirst(
                "ADMINISTRATIVE_INFORMATION_CONTRACT_UTILITIES" + " > CONDITIONS_OBTAINING_SPECIFICATIONS > TIME_LIMIT",
                procedureNode)))
            .setDocumentsPayable(JsoupUtils.exists(
                "ADMINISTRATIVE_INFORMATION_CONTRACT_UTILITIES > CONDITIONS_OBTAINING_SPECIFICATIONS"
                    + " > PAYABLE_DOCUMENTS", procedureNode).toString())
            .setDocumentsPrice(parseDocumentsPrice(JsoupUtils.selectFirst(
                "ADMINISTRATIVE_INFORMATION_CONTRACT_UTILITIES" + " > CONDITIONS_OBTAINING_SPECIFICATIONS"
                    + " > PAYABLE_DOCUMENTS > DOCUMENT_COST", procedureNode)))
            .setBidDeadline(TedTenderParserUtils.parseDateTime(
                JsoupUtils.selectFirst("ADMINISTRATIVE_INFORMATION_CONTRACT_UTILITIES > RECEIPT_LIMIT_DATE",
                    procedureNode)))
            .setEligibleBidLanguages(parseEligibleBidLanguages(
                JsoupUtils.select("ADMINISTRATIVE_INFORMATION_CONTRACT_UTILITIES > LANGUAGE > *",
                    procedureNode)))
            .addFunding(TedTenderParserUtils.parseFunding(complementaryInfoNode))
            .setAppealBodyName(JsoupUtils.selectText(
                "PROCEDURES_FOR_APPEAL > APPEAL_PROCEDURE_BODY_RESPONSIBLE > CONTACT_DATA_WITHOUT_RESPONSIBLE_NAME"
                    + " > ORGANISATION > OFFICIALNAME", complementaryInfoNode))
            .setMediationBodyName(JsoupUtils.selectText(
                "PROCEDURES_FOR_APPEAL > MEDIATION_PROCEDURE_BODY_RESPONSIBLE > CONTACT_DATA_WITHOUT_RESPONSIBLE_NAME"
                    + " > ORGANISATION > OFFICIALNAME", complementaryInfoNode));

        TedTenderParserUtils.appendNoticeReference(document, parsedTender);

        return parsedTender;
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
                "PROCEDURE_DEFINITION_CONTRACT_NOTICE_UTILITIES > ADMINISTRATIVE_INFORMATION_CONTRACT_UTILITIES"
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
            String sourceFormType;
            if (node.tagName().equalsIgnoreCase("OTHER_PREVIOUS_PUBLICATION")) {
                sourceFormType = node.tagName();
            } else {
                sourceFormType = JsoupUtils.selectAttribute("PRIOR_INFORMATION_NOTICE_F5", "CHOICE", node);
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
            JsoupUtils.selectFirst("NAME_ADDRESSES_CONTACT_CONTRACT_UTILITIES", buyerNode))
            .setBuyerType(TedTenderParserUtils.getDefaultBuyerType(buyerNode.ownerDocument()))
            .setMainActivities(TedTenderParserUtils.parseBuyerMainActivities(JsoupUtils.select(
                "ACTIVITY_OF_CONTRACTING_ENTITY, ACTIVITY_OF_CONTRACTING_ENTITY_OTHER",
                JsoupUtils.selectFirst("ACTIVITIES_OF_CONTRACTING_ENTITY", buyerNode)), buyerNode.ownerDocument()));
    }

    /**
     * Prases documents price.
     *
     * @param documentsPriceNode
     *         node with documents price data
     *
     * @return documents price
     */
    private static ParsedPrice parseDocumentsPrice(final Element documentsPriceNode) {
        if (documentsPriceNode == null) {
            return null;
        }

        return new ParsedPrice()
            .setNetAmount(documentsPriceNode.text())
            .setCurrency(documentsPriceNode.attr("CURRENCY"));
    }

    /**
     * Parses eligible languages.
     *
     * @param languageNodes
     *         language nodes
     *
     * @return list of eligible languages
     */
    private static List<String> parseEligibleBidLanguages(final Elements languageNodes) {
        if (languageNodes == null || languageNodes.isEmpty()) {
            return null;
        }

        final List<String> languages = new ArrayList<>();
        for (Element node : languageNodes) {
            final String lang;

            switch (node.tagName().toUpperCase()) {
                //attribute value contains ISO of EU language
                case "LANGUAGE_EC":
                    //attribute value contains string "YES", probably it means: Tenderer providing documents and/or
                    //certificates drafted in languages other than the EU official languages must provide a
                    // translation of
                    //these documents into one of these official languages.
                case "LANGUAGE_ANY_EC":
                    lang = JsoupUtils.selectAttribute("VALUE", node);
                    break;
                case "LANGUAGE_OTHER":
                    lang = node.text();
                    break;
                default:
                    continue;
            }

            languages.add(lang);
        }
        return languages;
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
}
