package eu.datlab.worker.eu.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
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
 * Parser for TED notice form.
 *
 * @author Michal Riha
 */
public final class TedContractNoticeHandler {

    /**
     * Source for TED publications.
     */
    private static final String PUBLICATIONS_SOURCE = PublicationSources.EU_TED;
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedContractNoticeHandler() {
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
        final Element contractingBodyNode = JsoupUtils.selectFirst("CONTRACTING_AUTHORITY_INFORMATION,"
            + "CONTRACTING_AUTHORITY_INFORMATION_DEFENCE", originNode);
        //II
        final Element contractNode = JsoupUtils.selectFirst("OBJECT_CONTRACT_INFORMATION, OBJECT_CONTRACT_INFORMATION_DEFENCE", originNode);
        //III
        final Element leftiNode = JsoupUtils.selectFirst("LEFTI_CONTRACT, LEFTI_CONTRACT_DEFENCE", originNode);
        //IV
        final Element procedureNode = JsoupUtils.selectFirst("PROCEDURE_DEFINITION_CONTRACT_NOTICE,"
            + "PROCEDURE_DEFINITION_CONTRACT_NOTICE_DEFENCE", originNode);
        //VI
        final Element complementaryInfoNode = JsoupUtils.selectFirst("COMPLEMENTARY_INFORMATION_CONTRACT_NOTICE",
                originNode);

        final Element onBehalfOfNode = JsoupUtils.selectFirst(
            "TYPE_AND_ACTIVITIES_AND_PURCHASING_ON_BEHALF > PURCHASING_ON_BEHALF > PURCHASING_ON_BEHALF_YES,"
            + "TYPE_AND_ACTIVITIES_OR_CONTRACTING_ENTITY_AND_PURCHASING_ON_BEHALF > PURCHASING_ON_BEHALF > PURCHASING_ON_BEHALF_YES",
                contractingBodyNode);

        final Element awardCriteriaNode =
            JsoupUtils.selectFirst("AWARD_CRITERIA_CONTRACT_NOTICE_INFORMATION > AWARD_CRITERIA_DETAIL", procedureNode);

        String procedureType = TedTenderParserUtils.parseProcedureType(
            JsoupUtils.selectFirst("TYPE_OF_PROCEDURE > TYPE_OF_PROCEDURE_DETAIL_FOR_CONTRACT_NOTICE,"
                + "TYPE_OF_PROCEDURE_DEFENCE > TYPE_OF_PROCEDURE_DETAIL_FOR_CONTRACT_NOTICE_DEFENCE", procedureNode));

        parsedTender
            .addPublication(parseMainPublication(originNode))
            .addPublications(parseOtherPublications(JsoupUtils.select(
                "PREVIOUS_PUBLICATION_NOTICE_F2, OTHER_PREVIOUS_PUBLICATIONS > OTHER_PREVIOUS_PUBLICATION, PREVIOUS_PUBLICATION_NOTICE_F17",
                JsoupUtils.selectFirst(
                    "ADMINISTRATIVE_INFORMATION_CONTRACT_NOTICE > PREVIOUS_PUBLICATION_INFORMATION_NOTICE_F2"
                    + " > PREVIOUS_PUBLICATION_EXISTS_F2, ADMINISTRATIVE_INFORMATION_CONTRACT_NOTICE_DEFENCE"
                    + " > PREVIOUS_PUBLICATION_INFORMATION_NOTICE_F17 > PREVIOUS_PUBLICATION_EXISTS_F17", procedureNode))))
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
            .setDescription(
                JsoupUtils.selectText("DESCRIPTION_CONTRACT_INFORMATION > SHORT_CONTRACT_DESCRIPTION,"
                    + "DESCRIPTION_CONTRACT_INFORMATION_DEFENCE > SHORT_CONTRACT_DESCRIPTION", contractNode))
            .setNationalProcedureType(procedureType)
            .setSupplyType(
                JsoupUtils.selectAttribute("TYPE_CONTRACT_LOCATION > TYPE_CONTRACT ", "VALUE", contractNode))
            .setAddressOfImplementation(TedTenderParserUtils.parseAddressOfImplementation(contractNode))
            .setIsFrameworkAgreement(
                JsoupUtils.exists("NOTICE_INVOLVES > ESTABLISHMENT_FRAMEWORK_AGREEMENT", contractNode).toString())
            .setIsDps(JsoupUtils.exists("NOTICE_INVOLVES > SETTING_UP_DPS", contractNode).toString())
            .setMaxFrameworkAgreementParticipants(
                JsoupUtils.selectText("F02_FRAMEWORK > SEVERAL_OPERATORS > MAX_NUMBER_PARTICIPANTS", contractNode))
            .setEstimatedDurationInDays(
                JsoupUtils.selectText("OPTIONS > PROVISIONAL_TIMETABLE_DAYS, PERIOD_WORK_DATE_STARTING > DAYS",
                    contractNode))
            .setEstimatedDurationInMonths(JsoupUtils.selectText(
                "F02_FRAMEWORK DURATION_FRAMEWORK_MONTH, OPTIONS > PROVISIONAL_TIMETABLE_MONTHS,"
                    + " PERIOD_WORK_DATE_STARTING > MONTHS", contractNode))
            .setEstimatedPrice(TedTenderParserUtils.parsePrice(
                JsoupUtils.selectFirst("QUANTITY_SCOPE > NATURE_QUANTITY_SCOPE > COSTS_RANGE_AND_CURRENCY",
                    contractNode)))
            .setCpvs(TedTenderParserUtils.parseCpvs(JsoupUtils.selectFirst("CPV", contractNode)))
            .setIsCoveredByGpa(
                JsoupUtils.hasAttribute(JsoupUtils.selectFirst("CONTRACT_COVERED_GPA", contractNode), "VALUE", "YES")
                    .toString())
            .setHasLots(
                Boolean.toString(!JsoupUtils.exists("F02_DIVISION_INTO_LOTS > DIV_INTO_LOT_NO,"
                    + "F17_DIVISION_INTO_LOTS > F17_DIV_INTO_LOT_NO", contractNode)))
            .setLots(parseLots(
                JsoupUtils.select("F02_DIVISION_INTO_LOTS > F02_DIV_INTO_LOT_YES > F02_ANNEX_B,"
                    + "F17_DIVISION_INTO_LOTS > F17_DIV_INTO_LOT_YES > F17_ANNEX_B", contractNode)))
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
                "F02_CONDITIONS_FOR_PARTICIPATION > ECONOMIC_OPERATORS_PERSONAL_SITUATION,"
                + "F17_CONDITIONS_FOR_PARTICIPATION > ECONOMIC_OPERATORS_PERSONAL_SITUATION", leftiNode))
            .setEconomicRequirements(
                JsoupUtils.selectCombinedText("F02_ECONOMIC_FINANCIAL_CAPACITY > *, F17_ECONOMIC_FINANCIAL_CAPACITY > *", leftiNode))
            .setTechnicalRequirements(JsoupUtils.selectCombinedText("TECHNICAL_CAPACITY_LEFTI > *", leftiNode))
            .setMaxBidsCount(JsoupUtils.selectText("MAXIMUM_NUMBER_INVITED > OPE_MAXIMUM_NUMBER", procedureNode))
            .setAwardCriteria(TedTenderParserUtils.parseAwardCritera(awardCriteriaNode))
            .setSelectionMethod(TedTenderParserUtils.parseSelectionMethod(codedDataNode))
            .setIsElectronicAuction(JsoupUtils.exists(
                "AWARD_CRITERIA_CONTRACT_NOTICE_INFORMATION > IS_ELECTRONIC_AUCTION_USABLE > USE_ELECTRONIC_AUCTION",
                procedureNode).toString())
            .setDocumentsDeadline(TedTenderParserUtils.parseDateTime(JsoupUtils.selectFirst(
                "ADMINISTRATIVE_INFORMATION_CONTRACT_NOTICE > CONDITIONS_OBTAINING_SPECIFICATIONS"
                    + " > TIME_LIMIT", procedureNode)))
            .setDocumentsPayable(JsoupUtils.exists(
                "ADMINISTRATIVE_INFORMATION_CONTRACT_NOTICE > CONDITIONS_OBTAINING_SPECIFICATIONS"
                    + " > PAYABLE_DOCUMENTS", procedureNode).toString())
            .setDocumentsPrice(parseDocumentsPrice(JsoupUtils.selectFirst(
                "ADMINISTRATIVE_INFORMATION_CONTRACT_NOTICE > CONDITIONS_OBTAINING_SPECIFICATIONS"
                    + " > PAYABLE_DOCUMENTS > DOCUMENT_COST", procedureNode)))
            .setBidDeadline(TedTenderParserUtils.parseDateTime(
                JsoupUtils.selectFirst("ADMINISTRATIVE_INFORMATION_CONTRACT_NOTICE > RECEIPT_LIMIT_DATE",
                    procedureNode)))
            .setEligibleBidLanguages(parseEligibleBidLanguages(
                JsoupUtils.select("ADMINISTRATIVE_INFORMATION_CONTRACT_NOTICE > LANGUAGE > *", procedureNode)))
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
                "PROCEDURE_DEFINITION_CONTRACT_NOTICE > ADMINISTRATIVE_INFORMATION_CONTRACT_NOTICE > FILE_REFERENCE_NUMBER,"
                + "PROCEDURE_DEFINITION_CONTRACT_NOTICE_DEFENCE > ADMINISTRATIVE_INFORMATION_CONTRACT_NOTICE_DEFENCE"
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
                sourceFormType = JsoupUtils.selectAttribute("PRIOR_INFORMATION_NOTICE_F2", "CHOICE", node);
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
