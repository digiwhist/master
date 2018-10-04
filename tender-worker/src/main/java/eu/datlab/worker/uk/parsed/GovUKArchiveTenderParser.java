package eu.datlab.worker.uk.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;
import org.jsoup.select.Elements;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import static eu.dl.worker.utils.jsoup.JsoupUtils.select;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectAttribute;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectText;
import java.util.stream.Collectors;

/**
 * Tender parser for GOV UK.
 *
 * @author Michal Riha
 */
public class GovUKArchiveTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1";

    private static final String NOTICE = "NOTICE";
    private static final String AWARD = "AWARD";
    private static final String PRIOR_INFORMATION = "PRIOR_INFORMATION";

    @Override
    public final List<ParsedTender> parse(final RawData rawData) {
        final Document document = Jsoup.parse(rawData.getSourceData(), "", Parser.xmlParser());
        List<ParsedTender> parsedTenders = new ArrayList<>();

        final Elements contracts = select("NOTICES > CONTRACT", document);
        final Elements contractAwards = select("NOTICES > CONTRACT_AWARD", document);
        final Elements priorInformations = select("NOTICES > PRIOR_INFORMATION", document);

        contracts.forEach(n -> {
            parsedTenders.add(parseTender(n, rawData.getSourceUrl().toString(), NOTICE));
        });

        contractAwards.forEach(n -> {
            parsedTenders.add(parseTender(n, rawData.getSourceUrl().toString(), AWARD));
        });

        priorInformations.forEach(n -> {
            parsedTenders.add(parseTender(n, rawData.getSourceUrl().toString(), PRIOR_INFORMATION));
        });

        return parsedTenders.isEmpty() ? null : parsedTenders;
    }

    /**
     * Encapsulate tender parsing to be used multiple times.
     *
     * @param element element to be parsed
     * @param url     url
     * @param sourceFormType     source form type
     *
     * @return ParsedTender
     */
    private ParsedTender parseTender(final Element element, final String url, final String sourceFormType) {
        String includedPublicationSourceId = selectNonEmptyText("NOTICE_ID", element);

        String procedureType = parseNationalProcedureType(element);

        final ParsedTender parsedTender = new ParsedTender()
            .setSize(selectNonEmptyAttribute("CONTRACTS_FINDER_NOTICE_GROUP", element))
            .setIsOnBehalfOf(selectNonEmptyAttribute("PURCHASING_ON_BEHALF", "VALUE", element))
            .setTitle(selectNonEmptyText("TITLE_CONTRACT", element))
            .setSupplyType(selectNonEmptyAttribute("TYPE_CONTRACT", "VALUE", element))
            .setDescription(selectNonEmptyText("SHORT_CONTRACT_DESCRIPTION", element))
            .setEstimatedStartDate(parseEstimatedStartDate(element))
            .setEstimatedCompletionDate(parseEstimatedCompletionDate(element))
            .setBidDeadline(parseBidDeadline(element))
            .setAwardDecisionDate(selectNonEmptyText("AwardedDate", element))
            .setBuyerAssignedId(selectNonEmptyText("file_reference_number", element))
            .setEstimatedDurationInDays(selectNonEmptyText("PERIOD_WORK_DATE_STARTING > DAYS", element))
            .setEstimatedDurationInMonths(selectNonEmptyText("PERIOD_WORK_DATE_STARTING > MONTHS", element))
            .setEstimatedDurationInYears(selectNonEmptyText("PERIOD_WORK_DATE_STARTING > YEARS", element))
            .setNationalProcedureType(procedureType)
            .setProcedureType(procedureType)
            .addFunding(new ParsedFunding()
                .setIsEuFund(String.valueOf(!JsoupUtils.exists("RELATES_TO_EU_PROJECT_NO", element))))
            .addPublication(new ParsedPublication()
                .setSource(PublicationSources.UK_GOV)
                .setIsIncluded(true)
                .setMachineReadableUrl(url)
                .setVersion(selectNonEmptyAttribute("CONTRACTS_FINDER_VERSION", element))
                .setSourceId(includedPublicationSourceId)
                .setSourceFormType(sourceFormType)
                .setHumanReadableUrl(selectNonEmptyText("URL_GENERAL", element))
                .setLanguage(selectNonEmptyAttribute("LANGUAGE_EC", "VALUE", element))
                .setDispatchDate(selectLocalDate("NOTICE_DISPATCH_DATE", element))
                .setPublicationDate(selectNonEmptyText("SYSTEM_PUBLISHED_DATE", element))
                .setIsValid(parseIsPublicationValid(element)))
            .addPublication(parsePublication("PARENT_NOTICE_ID", element))
            .addPublication(parsePublication("ROOT_NOTICE_ID", element))
            .addPublication(parsePublication("FILE_REFERENCE_NUMBER", element))
            .addPublication(includedPublicationSourceId == null ? null : new ParsedPublication()
                .setHumanReadableUrl("https://data.gov.uk/data/contracts-finder-archive/contract/"
                    + includedPublicationSourceId)
                .setSource(PublicationSources.UK_GOV)
                .setIsIncluded(false))
            .addBuyer(parseBuyer(JsoupUtils.selectFirst("CONTRACTING_AUTHORITY_INFORMATION,"
                + " AUTHORITY_PRIOR_INFORMATION", element)))
            .setAddressOfImplementation(new ParsedAddress()
                .setRawAddress(selectNonEmptyText("SITE_OR_LOCATION > LABEL > p", element))
                .addNuts(selectNonEmptyAttribute("NUTS", "CODE", element))
                .setCity(selectNonEmptyText("DELIVERY_LOCATION > TOWN", element))
                .setCountry(selectNonEmptyText("DELIVERY_LOCATION > COUNTRY", element))
                .setPostcode(selectNonEmptyText("DELIVERY_LOCATION > POSTAL_CODE", element)))
            .setEstimatedPrice(new ParsedPrice()
                .setCurrency(selectNonEmptyAttribute("COSTS_RANGE_AND_CURRENCY", "CURRENCY", element))
                .setMinNetAmount(selectNonEmptyText("RANGE_VALUE_COST > LOW_VALUE", element))
                .setMaxNetAmount(selectNonEmptyText("RANGE_VALUE_COST > HIGH_VALUE", element)))
            .addAwardCriterion(new ParsedAwardCriterion()
                .setName(selectNonEmptyText("CRITERIA", element))
                .setWeight(selectNonEmptyText("WEIGHTING", element)))
            .addLot(new ParsedTenderLot()
                .setStatus(selectNonEmptyText("Status", element))
                .addBid(!sourceFormType.equals(AWARD) ? null : new ParsedBid()
                    .setIsWinning(Boolean.TRUE.toString())
                    .setPrice(new ParsedPrice()
                        .setAmountWithVat(selectNonEmptyText("TOTAL_FINAL_VALUE > " +
                            "COSTS_RANGE_AND_CURRENCY_WITH_VAT_RATE > VALUE_COST", element))
                        .setCurrency(selectNonEmptyAttribute("TOTAL_FINAL_VALUE " +
                            "COSTS_RANGE_AND_CURRENCY_WITH_VAT_RATE", "CURRENCY", element)))
                    .addBidder(parseBody(JsoupUtils.selectFirst("ECONOMIC_OPERATOR_NAME_ADDRESS", element)))
                    .setIsSubcontracted(String.valueOf(!JsoupUtils.exists("NO_CONTRACT_LIKELY_SUB_CONTRACTED",
                        element))))
                .setAwardDecisionDate(selectLocalDate("CONTRACT_AWARD_DATE", element)))
            .addAwardCriterion(parseAwardCriterion(element))
            .setIsFrameworkAgreement(JsoupUtils.hasAttribute(JsoupUtils.selectFirst("FRAMEWORK_AGREEMENT", element),
                "VALUE", "YES").toString())
            .setEligibleBidLanguages(parseEligibleLanguages(element));


        // Parse CPVs
        final Elements cpvs = select("CPV_MAIN, CPV_ADDITIONAL", element);
        if (cpvs != null) {
            cpvs.forEach(n -> {
                String code = selectNonEmptyAttribute("CPV_CODE", "CODE", n);

                if (code != null && (parsedTender.getCpvs() == null || parsedTender.getCpvs().stream()
                        .noneMatch(t -> t.getCode().equals(code)))) {
                    parsedTender.addCpv(new ParsedCPV()
                        .setCode(code)
                        .setIsMain(String.valueOf(n.tagName().equalsIgnoreCase("CPV_MAIN"))));
                }
            });
        }

        return parsedTender;
    }

    /**
     * @param element
     *      element that includes aligible languages
     * @return non-empty list of languages or null
     */
    private List<String> parseEligibleLanguages(final Element element) {
        Elements languages = JsoupUtils.select("OTHER_LANGUAGE_VALUE", element);
        if (languages == null || languages.isEmpty()) {
            return null;
        }

        return languages.stream().map(n -> n.text()).collect(Collectors.toList());
    }

    /**
     * @param element
     *      element that includes body data
     * @return body or null
     */
    private ParsedBody parseBody(final Element element) {
        if (element == null) {
            return null;
        }

        return new ParsedBody()
            .setName(selectNonEmptyText(new String[]{"ORGANISATION", "BUYER_GROUP_NAME", "OrganisationName",
                "Organisation > Name"}, element))
            .setAddress(new ParsedAddress()
                .setStreet(selectNonEmptyText("ADDRESS", element))
                .setCity(selectNonEmptyText("TOWN", element))
                .setPostcode(selectNonEmptyText("POSTAL_CODE", element))
                .setUrl(selectNonEmptyText("URL", element)))
            .setContactName(selectNonEmptyText("CONTACT_POINT", element))
            .setPhone(selectNonEmptyText("PHONE", element))
            .setEmail(selectNonEmptyText("E_MAIL", element));
    }

    /**
     * @param element
     *      element that includes buyer data
     * @return buyer or null
     */
    private ParsedBody parseBuyer(final Element element) {
        ParsedBody buyer = parseBody(element);

        if (buyer != null) {
            buyer
                .addBodyId(parseBodyId(element))
                .setBuyerType(selectNonEmptyAttribute("type_of_contracting_authority_other", "value", element));
            buyer.getAddress().setUrl(selectNonEmptyText("URL_BUYER", element));
        }

        return buyer;
    }

    /**
     * Parse publication.
     *
     * @param selector selector
     * @param element element to parse from
     * @return ParsedPublication or null
     */
    private ParsedPublication parsePublication(final String selector, final Element element) {
        final String result = selectNonEmptyText(selector, element);

        return result == null ? null : new ParsedPublication()
                .setSourceId(result)
                .setSource(PublicationSources.UK_GOV)
                .setIsIncluded(false);
    }

    /**
     * Parse if publication is valid.
     *
     * @param element element to parse from
     * @return boolean
     */
    private boolean parseIsPublicationValid(final Element element) {
        final String noticeState = selectNonEmptyText("system_notice_state", element);

        return noticeState != null && noticeState.trim().equals("REPLACED");
    }

    /**
     * Parse national procedure type.
     *
     * @param element element to parse from
     * @return String or null
     */
    private String parseNationalProcedureType(final Element element) {
        Element procedureType = select("TYPE_OF_PROCEDURE > * > *, TYPE_OF_PROCEDURE_DEF > *", element).first();

        return procedureType == null ? null : procedureType.tagName();
    }

    /**
     * Parse award criterion.
     *
     * @param element element to parse from
     * @return String or null
     */
    private ParsedAwardCriterion parseAwardCriterion(final Element element) {
        Element criterion = select("AWARD_CRITERIA_CONTRACT_AWARD_NOTICE_INFORMATION > * > *", element).first();

        return criterion == null ? null : new ParsedAwardCriterion().setName(criterion.tagName());
    }

    /**
     * Parse body id.
     *
     * @param element element to parse from
     * @return BodyIdentifier or null
     */
    private BodyIdentifier parseBodyId(final Element element) {
        final String id = selectNonEmptyText("BUYER_GROUP_ID", element);

        return id == null ? null : new BodyIdentifier()
                .setId(id)
                .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                .setScope(BodyIdentifier.Scope.UNKNOWN);
    }

    /**
     * Parse estimated completion date.
     *
     * @param element element to parse from
     * @return String or null
     */
    private String parseEstimatedCompletionDate(final Element element) {

        String estimatedCompletionDate = selectLocalDate("END_DATE", element);

        if (estimatedCompletionDate == null) {
            estimatedCompletionDate = selectNonEmptyText("End", element);
        }

        return estimatedCompletionDate;
    }

    /**
     * Parse estimated start date.
     *
     * @param element element to parse from
     * @return String or null
     */
    private String parseEstimatedStartDate(final Element element) {
        String estimatedStartDate = selectLocalDate("START_DATE", element);

        if (estimatedStartDate == null) {
            estimatedStartDate = selectNonEmptyText("Start", element);
        }

        return estimatedStartDate;
    }

    /**
     * Parse bid deadline.
     *
     * @param element element to parse from
     * @return String or null
     */
    private String parseBidDeadline(final Element element) {
        String bidDeadline = selectLocalDateTime("RECEIPT_LIMIT_DATE", element);

        if (bidDeadline == null) {
            bidDeadline = selectNonEmptyText("DeadlineDate", element);
        }

        if (bidDeadline == null) {
            bidDeadline = selectNonEmptyText("RECEIPT_LIMIT_DATE_FOR", element);
        }

        return bidDeadline;
    }

    /**
     * Select text from attribute, return null if empty.
     *
     * @param elementSelector   selector
     * @param attributeSelector selector
     * @param element           element to parse
     *
     * @return String or null
     */
    private String selectNonEmptyAttribute(final String elementSelector, final String attributeSelector,
                                           final Element element) {
        final String result = selectAttribute(elementSelector, attributeSelector, element);
        return result == null || result.isEmpty() ? null : result;
    }

    /**
     * Select text from attribute, return null if empty.
     *
     * @param selector selector
     * @param element  element to parse
     *
     * @return String or null
     */
    private String selectNonEmptyAttribute(final String selector, final Element element) {
        final String result = selectAttribute(selector, element);
        return result == null || result.isEmpty() ? null : result;
    }

    /**
     * Select text from element, return null if empty.
     *
     * @param selector selector
     * @param element  element to parse
     *
     * @return String or null
     */
    private String selectNonEmptyText(final String selector, final Element element) {
        final String result = selectText(selector, element);
        return result == null || result.isEmpty() ? null : result;
    }

    /**
     * Select text from element, return null if empty.
     *
     * @param selectors selectors
     * @param element   element to parse
     *
     * @return String or null
     */
    private String selectNonEmptyText(final String[] selectors, final Element element) {
        for (String selector : selectors) {
            final String result = selectText(selector, element);
            if (result != null && !result.isEmpty()) {
                return result;
            }
        }

        return null;
    }

    /**
     * Create date string from selected element in the given context.
     *
     * @param selector
     *      date node selector (holds nodes DAY, MONTH and YEAR)
     * @param element
     *      element to be parsed from
     * @return date string or null
     */
    private String selectLocalDate(final String selector, final Element element) {
        LocalDateTime datetime = parseDateTime(JsoupUtils.selectFirst(selector, element));
        return datetime == null ? null : datetime.toLocalDate().toString();
    }

    /**
     * Create date time string from selected element in the given context.
     *
     * @param selector
     *      datetime node selector (holds nodes DAY, MONTH, YEAR and TIME)
     * @param element
     *      element to be parsed from
     * @return date time string or null
     */
    private String selectLocalDateTime(final String selector, final Element element) {
        LocalDateTime datetime = parseDateTime(JsoupUtils.selectFirst(selector, element));
        return datetime == null ? null : datetime.toString();
    }

    /**
     * @param element
     *      element that includes date(time) nodes (YEAR, MONTH, DAY, and optionally TIME).
     * @return local date time or null
     */
    private LocalDateTime parseDateTime(final Element element) {
        String day = selectNonEmptyText("DAY", element);
        String month = selectNonEmptyText("MONTH", element);
        String year = selectNonEmptyText("YEAR", element);
        String time = selectNonEmptyText("TIME", element);

        if (day == null && month == null && year == null) {
            return null;
        }

        Integer hours = 0, minutes = 0;
        if (time != null) {
            String[] hourAndMinutes = time.split(":");
            hours = Integer.valueOf(hourAndMinutes[0]);
            minutes = Integer.valueOf(hourAndMinutes[1]);
        }

        return LocalDateTime.of(Integer.valueOf(year), Integer.valueOf(month), Integer.valueOf(day), hours, minutes);
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "UK";
    }
}
