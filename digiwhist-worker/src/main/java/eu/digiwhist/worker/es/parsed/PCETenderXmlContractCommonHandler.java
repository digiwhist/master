package eu.digiwhist.worker.es.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedDocument;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static eu.dl.worker.utils.jsoup.JsoupUtils.select;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectAttribute;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectText;

/**
 * Parser handler used for tender xml contract notice parsing.
 */
public final class PCETenderXmlContractCommonHandler {
    /**
     * Suppress default constructor for noninstantiability.
     */
    private PCETenderXmlContractCommonHandler() {
    }

    /**
     * Parses tender XML detail.
     *
     * @param parsedTender parsed tender
     * @param document     parsed document
     * @param tenderUrl    tender Url
     * @param formType     tender type
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document, final String tenderUrl,
                                     final String formType) {
        parsedTender
                .setBuyerAssignedId(selectText("cbc|ContractFolderID", document))
                .addPublication(new ParsedPublication()
                        .setSourceTenderId(selectText("cbc|ContractFolderID", document))
                        .setSourceId(selectText("cbc|UUID", document))
                        .setIsIncluded(true)
                        .setPublicationDate(selectText("cbc|IssueDate", document))
                        .setMachineReadableUrl(tenderUrl)
                        .setSourceFormType(formType)
                        .setSource(PublicationSources.ES_PCE))
                .addBuyer(new ParsedBody()
                        .setMainActivities(parseMainActivities(document))
                        .setContactPoint(selectText("cac|Person cbc|JobTitle", document))
                        .setAddress(new ParsedAddress()
                                .setUrl(selectText("cac|Party cbc|WebsiteURI", document))
                                .setCity(selectText("cac|Party cbc|CityName", document))
                                .setPostcode(selectText("cac|Party cbc|PostalZone", document))
                                .setStreet(selectText("cac|Party cbc|Line", document))
                                .setCountry(selectText("cac|Party cac|Country cbc|Name", document)))
                        .addBodyId(parseOrganizationId(document))
                        .addBodyId(parseTaxId(document))
                        .setName(selectText("cac|Party cbc|Name", document))
                        .setPhone(selectText("cac|Party cbc|Telephone", document))
                        .setEmail(selectText("cac|Party cbc|ElectronicMail", document)))
                .setDeposits(selectText("cbc|GuaranteeTypeCode", document))
                .setEligibilityCriteria(selectText("cbc|Description", document))
                .addAwardCriterion(new ParsedAwardCriterion()
                        .setName(selectText("cac|AwardingCriteria cbc|ID", document))
                        .setDescription(selectText("cac|AwardingCriteria cbc|Description", document))
                        .setWeight(selectText("cac|AwardingCriteria cbc|WeightNumeric", document)))
                .setSpecificationsProvider(new ParsedBody()
                        .setName(selectText("cac|AdditionalInformationParty cbc|Name", document))
                        .setPhone(selectText("cac|AdditionalInformationParty cbc|Telephone", document))
                        .setAddress(new ParsedAddress()
                                .setCity(selectText("cac|AdditionalInformationParty cbc|CityName", document))
                                .setPostcode(selectText("cac|AdditionalInformationParty cbc|PostalZone",
                                        document))
                                .setStreet(selectText("cac|AdditionalInformationParty cbc|Line", document))
                                .setCountry(selectText("cac|AdditionalInformationParty cac|Country " +
                                        "cbc|Name", document))))
                .addAdministrator(new ParsedBody()
                        .setName(selectText("cac|TenderRecipientParty cbc|Name", document))
                        .setAddress(new ParsedAddress()
                                .setCity(selectText("cac|TenderRecipientParty cbc|CityName", document))
                                .setPostcode(selectText("cac|TenderRecipientParty cbc|PostalZone", document))
                                .setStreet(selectText("cac|TenderRecipientParty cbc|Line", document))
                                .setCountry(selectText("cac|TenderRecipientParty cac|Country cbc|Name", document))))
                .setDocuments(
                        Arrays.asList(new ParsedDocument()
                                .setLanguage(selectText("cac|Language cbc|ID", document))
                                .setUrl(selectText("cac|Attachment cbc|URI", document))))
                .setProcedureType(selectText("cbc|ProcedureCode", document))
                .setNationalProcedureType(selectAttribute("cbc|ProcedureCode", "name", document))
                .setIsElectronicAuction(selectText("cbc|SubmissionMethodCode", document))
                .setDocumentsDeadline(parseDocumentsDeadline(document))
                .setBidDeadline(parseBidDeadline(document))
                .setSupplyType(selectText("cac|ProcurementProject cbc|TypeCode", document))
                .setTitle(selectText("cac|ProcurementProject cbc|Name", document))
                .setEstimatedPrice(new ParsedPrice()
                        .setAmountWithVat(selectText("cac|ProcurementProject cbc|TotalAmount", document))
                        .setNetAmount(selectText("cac|ProcurementProject cbc|TaxExclusiveAmount", document))
                        .setCurrency(selectAttribute("cac|ProcurementProject cbc|TaxExclusiveAmount", "currencyID",
                                document)))
                .addCpv(new ParsedCPV()
                        .setCode(selectText("cac|ProcurementProject cbc|ItemClassificationCode", document))
                        .setIsMain(Boolean.TRUE.toString()))
                .setAddressOfImplementation(new ParsedAddress()
                        .addNuts(selectText("cac|ProcurementProject cbc|CountrySubentityCode", document)))
                .setLots(parseLots(document))
                .setFundings(parseFundings(document));

        final String estimatedDurationUnit = selectAttribute("cac|ProcurementProject cbc|DurationMeasure",
                "unitCode", document);

        if (estimatedDurationUnit != null) {
            if (estimatedDurationUnit.contains("DAY")) {
                parsedTender.setEstimatedDurationInDays(selectText("cac|ProcurementProject " +
                        "cbc|DurationMeasure", document));
            } else if (estimatedDurationUnit.contains("MONTH")) {
                parsedTender.setEstimatedDurationInMonths(selectText("cac|ProcurementProject " +
                        "cbc|DurationMeasure", document));
            } else if (estimatedDurationUnit.contains("YEAR")) {
                parsedTender.setEstimatedDurationInYears(selectText("cac|ProcurementProject " +
                        "cbc|DurationMeasure", document));
            }
        }

        return parsedTender;
    }

    /**
     * Parse organization id.
     *
     * @param document document to parse from
     *
     * @return BodyIdentifier or null
     */
    private static BodyIdentifier parseOrganizationId(final Document document) {
        final BodyIdentifier bodyIdentifier = new BodyIdentifier()
                .setId(selectText("cac|Party cbc|ID[schemeName=ID_PLATAFORMA]", document))
                .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                .setScope(BodyIdentifier.Scope.ES);

        return bodyIdentifier.getId() == null ? null : bodyIdentifier;
    }

    /**
     * Parse organization id.
     *
     * @param document document to parse from
     *
     * @return BodyIdentifier or null
     */
    private static BodyIdentifier parseTaxId(final Document document) {
        final BodyIdentifier bodyIdentifier = new BodyIdentifier()
                .setId(selectText("cac|Party cbc|ID[schemeName=NIF]", document))
                .setType(BodyIdentifier.Type.TAX_ID)
                .setScope(BodyIdentifier.Scope.ES);

        return bodyIdentifier.getId() == null ? null : bodyIdentifier;
    }

    /**
     * Parse bid deadline date and time.
     *
     * @param document document to parse from
     *
     * @return String or null
     */
    private static String parseBidDeadline(final Document document) {

        String date = selectText("cac|ParticipationRequestReceptionPeriod cbc|EndDate", document);
        String time = selectText("cac|ParticipationRequestReceptionPeriod cbc|EndTime", document);

        if (date == null) {
            date = selectText("cac|DocumentAvailabilityPeriod cbc|EndDate", document);
            time = selectText("cac|DocumentAvailabilityPeriod cbc|EndTime", document);
        }


        if (date != null && time != null) {
            return date.replaceAll("\\+.*", "T" + time);
        }

        return null;
    }

    /**
     * Parse documents deadline date and time.
     *
     * @param document document to parse from
     *
     * @return String or null
     */
    private static String parseDocumentsDeadline(final Document document) {
        final String date = selectText("cac|DocumentAvailabilityPeriod cbc|EndDate", document);
        final String time = selectText("cac|DocumentAvailabilityPeriod cbc|EndTime", document);


        if (date != null && time != null) {
            return date.replaceAll("\\+.*", "T" + time);
        }

        return null;
    }

    /**
     * Parse main activities of buyer.
     *
     * @param document document to parse from
     *
     * @return List<String> or null
     */
    private static List<String> parseMainActivities(final Document document) {
        final Elements activities = select("cac|ContractingParty cbc|ActivityCode", document);

        if (activities != null && !activities.isEmpty()) {
            final List<String> parsedActivities = new ArrayList<>();

            for (Element activity : activities) {
                parsedActivities.add(activity.text());
            }

            return parsedActivities;
        }

        return null;
    }

    /**
     * Parse lots if any.
     *
     * @param document document to parse from
     *
     * @return List<ParsedTenderLot> or null
     */
    private static List<ParsedTenderLot> parseLots(final Document document) {
        final Elements lots = document.select("cac|ProcurementProjectLot");

        final List<ParsedTenderLot> parsedLots = new ArrayList<>();
        if (lots != null && !lots.isEmpty()) {
            for (Element lot : lots) {
                parsedLots.add(new ParsedTenderLot()
                        .setLotNumber(selectText("cbc|ID", lot))
                        .setPositionOnPage(String.valueOf(parsedLots.size() + 1))
                        .setTitle(selectText("cbc|Name", lot))
                        .addCpv(new ParsedCPV()
                                .setCode(selectText("cbc|ItemClassificationCode", lot)))
                        .setEstimatedPrice(new ParsedPrice()
                                .setAmountWithVat(selectText("cbc|TotalAmount", lot))
                                .setNetAmount(selectText("cbc|TaxExclusiveAmount", lot))));
            }
        }

        return parsedLots.isEmpty() ? null : parsedLots;
    }

    /**
     * Parses list of fundings.
     *
     * @param document document to parse from
     *
     * @return non-empty list of fundings or null
     */
    private static List<ParsedFunding> parseFundings(final Document document) {
        final String fundingProgramCode = selectText("cbc|FundingProgramCode", document);
        final String source = selectText("cbc|FundingProgram", document);

        return fundingProgramCode == null && source == null
                ? null
                : new ArrayList<>(Collections.singletonList(new ParsedFunding()
                .setIsEuFund(fundingProgramCode == null ? null : Boolean.toString(fundingProgramCode.equals("EU")))
                .setSource(source)));
    }
}
