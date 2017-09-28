package eu.digiwhist.worker.uk.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
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
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;
import org.jsoup.select.Elements;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import static eu.dl.worker.utils.jsoup.JsoupUtils.select;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectAttribute;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectText;

/**
 * Tender parser for GOV UK.
 *
 * @author Michal Riha
 */
public class GovUKArchiveTenderParser extends BaseDigiwhistTenderParser {
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

        for (Element contract : contracts) {
            parsedTenders.add(parseTender(contract, rawData.getSourceUrl().toString(), NOTICE));
        }

        for (Element contractAward : contractAwards) {
            parsedTenders.add(parseTender(contractAward, rawData.getSourceUrl().toString(), AWARD));
        }

        for (Element priorInformation : priorInformations) {
            parsedTenders.add(parseTender(priorInformation, rawData.getSourceUrl().toString(), PRIOR_INFORMATION));
        }

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
        final String includedPublicationSourceId = selectNonEmptyText("NOTICE_ID", element);

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
                .setNationalProcedureType(parseNationalProcedureType(element))
                .addFunding(new ParsedFunding()
                        .setIsEuFund(String.valueOf(select("RELATES_TO_EU_PROJECT_NO", element).first() != null)))
                .addPublication(new ParsedPublication()
                        .setSource(PublicationSources.UK_GOV)
                        .setIsIncluded(true)
                        .setMachineReadableUrl(url)
                        .setVersion(selectNonEmptyAttribute("CONTRACTS_FINDER_VERSION", element))
                        .setSourceId(includedPublicationSourceId)
                        .setSourceFormType(sourceFormType)
                        .setHumanReadableUrl(selectNonEmptyText("URL_GENERAL", element))
                        .setLanguage(selectNonEmptyAttribute("LANGUAGE_EC", "VALUE", element))
                        .setDispatchDate(selectLocalDate("NOTICE_DISPATCH_DATE > DAY",
                                "NOTICE_DISPATCH_DATE > MONTH", "NOTICE_DISPATCH_DATE > YEAR", element))
                        .setPublicationDate(selectNonEmptyText("SYSTEM_PUBLISHED_DATE", element))
                        .setIsValid(parseIsPublicationValid(element)))
                .addPublication(parsePublication("PARENT_NOTICE_ID", element))
                .addPublication(parsePublication("ROOT_NOTICE_ID", element))
                .addPublication(includedPublicationSourceId == null ? null : new ParsedPublication()
                        .setHumanReadableUrl("https://data.gov.uk/data/contracts-finder-archive/contract/"
                                + includedPublicationSourceId)
                        .setSource(PublicationSources.UK_GOV)
                        .setIsIncluded(false))
                .addBuyer(new ParsedBody()
                        .setName(selectNonEmptyText(new String[]{"ORGANISATION", "BUYER_GROUP_NAME", "OrganisationName",
                                "Organisation > Name"}, element))
                        .setAddress(new ParsedAddress()
                                .setStreet(selectNonEmptyText("ADDRESS", element))
                                .setCity(selectNonEmptyText("TOWN", element))
                                .setPostcode(selectNonEmptyText("POSTAL_CODE", element))
                                .setUrl(selectNonEmptyText("URL_BUYER", element)))
                        .setContactName(selectNonEmptyText("CONTACT_POINT", element))
                        .setPhone(selectNonEmptyText("PHONE", element))
                        .setEmail(selectNonEmptyText("E_MAIL", element))
                        .setBuyerType(selectNonEmptyAttribute("type_of_contracting_authority_other", "value", element))
                        .addBodyId(parseBodyId(element)))
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
                                                "COSTS_RANGE_AND_CURRENCY_WITH_VAT_RATE", "CURRENCY", element))
                                )));

        // Parse main CPVs
        final Elements mainCpvs = select("CPV_MAIN", element);
        if (mainCpvs != null) {
            for (Element mainCpv : mainCpvs) {
                parsedTender.addCpv(new ParsedCPV()
                        .setCode(selectNonEmptyAttribute("CPV_CODE", "CODE", mainCpv))
                        .setIsMain(String.valueOf(true)));
            }
        }

        // Parse additional CPVs
        final Elements additionalCpvs = select("CPV_ADDITIONAL", element);
        if (additionalCpvs != null) {
            for (Element additionalCpv : additionalCpvs) {
                final String code = selectNonEmptyAttribute("CPV_CODE", "CODE", additionalCpv);

                // Code is added only if its not already there
                if (code != null && parsedTender.getCpvs().stream().noneMatch(t -> t.getCode().equals(code))) {
                    parsedTender.addCpv(new ParsedCPV()
                            .setCode(code)
                            .setIsMain(String.valueOf(false)));
                }
            }
        }

        return parsedTender;
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
        Element procedureType = select("TYPE_OF_PROCEDURE > * > *", element).first();

        return procedureType == null ? null : procedureType.tagName();
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

        String estimatedCompletionDate = selectLocalDate("END_DATE > DAY", "END_DATE > MONTH", "END_DATE > YEAR",
                element);

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
        String estimatedStartDate = selectLocalDate(
                "START_DATE > DAY", "START_DATE > MONTH", "START_DATE > YEAR", element);

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
        String bidDeadline = selectLocalDateTime("RECEIPT_LIMIT_DATE > DAY", "RECEIPT_LIMIT_DATE > MONTH",
                "RECEIPT_LIMIT_DATE > YEAR", "RECEIPT_LIMIT_DATE > TIME", element);

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
     * Create date string from selectors.
     *
     * @param daySelector   daySelector
     * @param monthSelector monthSelector
     * @param yearSelector  yearSelector
     * @param element       element to be parsed from
     *
     * @return String or null
     */
    private String selectLocalDate(final String daySelector, final String monthSelector, final String yearSelector,
                                   final Element element) {
        final String day = selectNonEmptyText(daySelector, element);
        final String month = selectNonEmptyText(monthSelector, element);
        final String year = selectNonEmptyText(yearSelector, element);

        if (day != null && month != null && year != null) {
            return String.valueOf(LocalDate.of(Integer.valueOf(year), Integer.valueOf(month), Integer.valueOf(day)));
        }

        return null;
    }

    /**
     * Create date string from selectors.
     *
     * @param daySelector   daySelector
     * @param monthSelector monthSelector
     * @param yearSelector  yearSelector
     * @param timeSelector  timeSelector
     * @param element       element to be parsed from
     *
     * @return String or null
     */
    private String selectLocalDateTime(final String daySelector, final String monthSelector, final String yearSelector,
                                       final String timeSelector, final Element element) {
        final String day = selectNonEmptyText(daySelector, element);
        final String month = selectNonEmptyText(monthSelector, element);
        final String year = selectNonEmptyText(yearSelector, element);
        final String time = selectNonEmptyText(timeSelector, element);

        if (day != null && month != null && year != null && time != null) {
            final String[] hourAndMinutes = time.split(":");

            if (hourAndMinutes.length > 1) {
                final String hour = hourAndMinutes[0];
                final String minutes = hourAndMinutes[1];

                if (!hour.isEmpty() && !minutes.isEmpty()) {
                    return String.valueOf(LocalDateTime.of(Integer.valueOf(year), Integer.valueOf(month),
                            Integer.valueOf(day), Integer.valueOf(hour), Integer.valueOf(minutes)));
                }
            }
        }

        return null;
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
