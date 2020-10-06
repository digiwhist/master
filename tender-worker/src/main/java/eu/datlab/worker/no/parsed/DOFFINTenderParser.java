package eu.datlab.worker.no.parsed;

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
import eu.dl.worker.utils.ArrayUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Created by michalriha on 02/05/2017.
 */
public class DOFFINTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    @Override
    public final List<ParsedTender> parse(final RawData raw) {
        final Document document = Jsoup.parse(raw.getSourceData(), "", Parser.xmlParser());

        Element buyer = JsoupUtils.selectFirst("CONTRACTING_AUTHORITY_VEAT, CONTRACTING_BODY,"
            + " CA_CE_CONCESSIONAIRE_PROFILE, ADDRESS_AUTHORITY", document);
        
        String publicationDate = (String) raw.getMetaData().get("publicationDate");

        String procedureType = JsoupUtils.selectText("TYPE_OF_PROCEDURE_DEF_F15", document);
        if (procedureType == null) {
            Element procedureTypeNode = JsoupUtils.selectFirst("TYPE_OF_PROCEDURE_DEF,"
                + " TYPE_OF_PROCEDURE_DETAIL_FOR_CONTRACT_NOTICE, PROCEDURE", document);

            if (procedureTypeNode != null && procedureTypeNode.childNodeSize() > 0) {
                procedureType = procedureTypeNode.child(0).nodeName();
            }
        }

        ParsedAddress documentsLocation = parseDocumentsLocation(document);

        ParsedPrice estimatedPrice = parseInlinePrice(JsoupUtils.selectFirst("VAL_ESTIMATED_TOTAL", document));
        if(estimatedPrice == null){
            estimatedPrice = parsePrice(JsoupUtils.selectFirst("COSTS_RANGE_AND_CURRENCY", document));
        }

        return Collections.singletonList(new ParsedTender()
            .setPublications(parsePublications(document, raw.getSourceUrl().toString(), publicationDate))
            .addBuyer(parseBuyer(document,
                    JsoupUtils.selectFirst("CONTRACTING_AUTHORITY_VEAT, CONTRACTING_BODY,"
                + " CA_CE_CONCESSIONAIRE_PROFILE, ADDRESS_AUTHORITY", buyer)))
            .setTitle(JsoupUtils.selectText("TITLE_CONTRACT, TITLE, NAME_GIVEN_TO_CONTRACT", document))
            .setSupplyType(parseSupplyType(document))
            .setAddressOfImplementation(parseAddressOfImplementation(document))
            .setDescription(JsoupUtils.selectText("SHORT_CONTRACT_DESCRIPTION, *:not(OBJECT_DESCR) > SHORT_DESCR," +
                            " DESCRIPTION_OF_CONTRACT",
                document))
            .setIsCoveredByGpa(parseBoolean("*:not(OBJECT_DESCR) > CONTRACT_COVERED_GPA", document))
            .setFinalPrice(parseTenderFinalPrice(document))
            .setNationalProcedureType(procedureType)
            .setLots(parseLots(document))
            .addNpwpReason(JsoupUtils.selectTagName("REASONS_PROVIDED_PARTICULAR_TENDERER > *", document))
            .setDocumentsLocation(documentsLocation)
            .setIsOnBehalfOf(parseBoolean("PURCHASING_ON_BEHALF, PURCHASING_ON_BEHALF_YES", document))
            .setOnBehalfOf(parseOnBehalfOf(document))
            .setSelectionMethod(JsoupUtils.selectTagName("AWARD_CRITERIA_DETAIL_F03 > *, AWARD_CRITERIA_DETAIL > *",
                document))
            .setBuyerAssignedId(JsoupUtils.selectText("FILE_REFERENCE_NUMBER, REFERENCE_NUMBER", document))
            .setEstimatedStartDate(parseDate("PERIOD_WORK_DATE_STARTING > INTERVAL_DATE > START_DATE", document))
            .setEstimatedCompletionDate(parseDate("PERIOD_WORK_DATE_STARTING > INTERVAL_DATE > END_DATE", document))
            .setEligibleBidLanguages(parseEligibleBidLanguages(document))
            .setAreVariantsAccepted(parseBoolean("ACCEPTED_VARIANTS", document))
            .setHasOptions(parseBoolean("*:not(OBJECT_DESCR) > OPTIONS", document))
            .setIsElectronicAuction(parseBoolean("*:not(OBJECT_DESCR) > USE_ELECTRONIC_AUCTION", document))
            .setIsFrameworkAgreement(parseIsFrameworkAgreement(document))
            .setEstimatedDurationInYears(JsoupUtils.selectText("DURATION_FRAMEWORK_YEAR", document))
            .setAwardCriteria(parseAwardCriteria(document))
            .setCpvs(parseCPVs(document))
            .setTechnicalRequirements(JsoupUtils.selectText("TECHNICAL_CAPACITY_LEFTI,"
                + " LEFTI TECHNICAL_CRITERIA", document))
            .setPersonalRequirements(JsoupUtils.selectText("ECONOMIC_OPERATORS_PERSONAL_SITUATION", document))
            .setEconomicRequirements(JsoupUtils.selectText("F02_ECONOMIC_FINANCIAL_CAPACITY,"
                + " LEFTI ECONOMIC_CRITERIA", document))
            .setFurtherInformationProvider(JsoupUtils.exists("ADDRESS_FURTHER_INFO_IDEM", document)
                ? new ParsedBody().setAddress(documentsLocation)
                : parseBody(JsoupUtils.selectFirst("FURTHER_INFORMATION", document)))
            .setSpecificationsProvider(parseBody(JsoupUtils.selectFirst("SPECIFICATIONS_AND_ADDITIONAL_DOCUMENTS",
                document)))
            .setBidsRecipient(JsoupUtils.exists("ADDRESS_PARTICIPATION_IDEM", document)
                ? new ParsedBody().setAddress(documentsLocation)
                : parseBody(JsoupUtils.selectFirst("TENDERS_REQUESTS_APPLICATIONS_MUST_BE_SENT_TO", document)))
            .setDocumentsDeadline(parseDate("CONDITIONS_OBTAINING_SPECIFICATIONS", document))
            .setDocumentsPayable(parseBoolean("PAYABLE_DOCUMENTS", document))
            .setBidDeadline(parseBidDeadLine(document))
            .setAppealBodyName(JsoupUtils.selectText("APPEAL_PROCEDURE_BODY_RESPONSIBLE ORGANISATION,"
                + " ADDRESS_REVIEW_BODY OFFICIALNAME", document))
            .addFunding(parseFunding(document))
            .setAwardDeadlineDuration(parseAwardDeadlineDuration(document))
            .setAwardDeadline(JsoupUtils.selectText("DATE_TENDER_VALID", document))
            .setHasLots(String.valueOf(!JsoupUtils.exists("DIV_INTO_LOT_NO, DIVISION_INTO_LOTS_NO", document)))
            .setEstimatedDurationInMonths(JsoupUtils.selectText("PERIOD_WORK_DATE_STARTING MONTHS", document))
            .setEnvisagedMaxCandidatesCount(JsoupUtils.selectText("NB_PARTICIPANTS", document))
            .setEligibilityCriteria(JsoupUtils.selectText("LEFTI SUITABILITY", document))
            .setIsEInvoiceAccepted(parseBoolean("EINVOICING", document))
            .setEstimatedPrice(estimatedPrice));
    }

    /**
     * @param document
     *      document to parse from
     * @return string "true", "false" or NULL
     */
    private String parseIsFrameworkAgreement(final Document document) {
        String bool = parseBoolean("ESTABLISHMENT_FRAMEWORK_AGREEMENT, F02_FRAMEWORK, *:not(OBJECT_DESCR) > FRAMEWORK, "
            + "FRAMEWORK_AGREEMENT", document);

        Element node = JsoupUtils.selectFirst("NOTICE_INVOLVES_DESC", document);
        if (bool == null && node != null) {
            return JsoupUtils.hasAttribute(node, "VALUE", "CONCLUSION_FRAMEWORK_AGREEMENT").toString();
        }
        
        return null;
    }



    /**
     * @param document
     *      document to parse from
     * @return bid deadline
     */
    private String parseBidDeadLine(final Document document) {
        String date = parseDate("RECEIPT_LIMIT_DATE, DEADLINE_TENDER_DATE", document);
        if (date == null) {
            date = JsoupUtils.selectText("DATE_RECEIPT_TENDERS", document, true) + " "
                + JsoupUtils.selectText("TIME_RECEIPT_TENDERS", document, true);
        }

        return date;
    }

    /**
     * @param document
     *      document to parse from
     * @return award deadline duration in days
     */
    private String parseAwardDeadlineDuration(final Document document) {
        Element node = JsoupUtils.selectFirst("MINIMUM_TIME_MAINTAINING_TENDER > *", document);

        if (node == null) {
            return null;
        }

        Integer value;
        try {
            value = Integer.valueOf(node.text());
        } catch (NumberFormatException ex) {
            logger.warn("Award deadline duration value '{}' is not a number", node.text());
            return null;
        }

        if (node.nodeName().equalsIgnoreCase("PERIOD_MONTH")) {
            value *= 30;
        } else if (node.nodeName().equalsIgnoreCase("PERIOD_YEAR")) {
            value *= 365;
        }
        
        return value.toString();
    }

    /**
     * Parses fundings.
     *
     * @param context
     *      context to parse from
     * @return funding
     */
    private static ParsedFunding parseFunding(final Element context) {
        if (context == null) {
            return null;
        }
        
        return new ParsedFunding()
            .setIsEuFund(JsoupUtils.exists("EU_PROGR_RELATED", context).toString())
            .setProgramme(JsoupUtils.selectText("EU_PROGR_RELATED", context));
    }

    /**
     * @param context
     *      context to parse from
     * @return list of prased CPVs
     */
    private List<ParsedCPV> parseCPVs(final Element context) {
        return JsoupUtils.select("CPV_MAIN, CPV_ADDITIONAL", context).stream()
            .map(n -> {
                return new ParsedCPV()
                    .setCode(JsoupUtils.selectAttribute("CPV_CODE", "CODE", n))
                    .setIsMain(String.valueOf(n.tagName().equalsIgnoreCase("CPV_MAIN")));
            }).collect(Collectors.toList());
    }

    /**
     * @param document
     *      document to parse from
     * @return non empty list of parsed criteria or null
     */
    private List<ParsedAwardCriterion> parseAwardCriteria(final Document document) {
        List<ParsedAwardCriterion> criteria = new ArrayList<>();

        JsoupUtils.select("AC_QUALITY, AC_PRICE", document).stream()
            .forEach(n -> {
                boolean isPrice = n.tagName().equalsIgnoreCase("AC_PRICE");
                criteria.add(new ParsedAwardCriterion()
                    .setName(isPrice ? "price" : JsoupUtils.selectText("AC_CRITERION", n))
                    .setWeight(JsoupUtils.selectText("AC_WEIGHTING", n))
                    .setIsPriceRelated(String.valueOf(isPrice)));
            });

        JsoupUtils.select("CRITERIA_DEFINITION", document).stream()
            .forEach(n -> {
                String name = JsoupUtils.selectText("CRITERIA", n);
                criteria.add(new ParsedAwardCriterion()
                    .setName(name)
                    .setWeight(JsoupUtils.selectText("WEIGHTING", n))
                    .setIsPriceRelated(String.valueOf("pris".equalsIgnoreCase(name))));
            });

        return criteria.isEmpty() ? null : criteria;
    }

    /**
     *
     * @param selector
     *      TRUE element selector
     * @param context
     *      context to be searched
     * @return string "true", "false", or NULL
     */
    private String parseBoolean(final String selector, final Element context) {
        Element node = JsoupUtils.selectFirst(selector, context);
        if (node == null) {
            Element falseNode = JsoupUtils.selectFirst(selector.replaceAll("(\\w+)$", "NO_$1"), context);
            return falseNode != null ? String.valueOf(false) : null;
        }

        if (node.hasAttr("VALUE")) {
            return String.valueOf(node.attr("VALUE").equals("YES"));
        } else {
            return String.valueOf(true);
        }
    }

    /**
     * @param document
     *      document to parse from
     * @return string "true" or "false"
     */
    private String parseIsOnBehalfOf(final Document document) {
        Element node = JsoupUtils.selectFirst("PURCHASING_ON_BEHALF", document);
        if (node == null) {
            return JsoupUtils.exists("PURCHASING_ON_BEHALF_YES", document).toString();
        }

        return String.valueOf(Objects.equals(node.attr("VALUE"), "YES"));
    }


    /**
     * @param document
     *      document to parsed from
     * @return list of eligible languages
     */
    private List<String> parseEligibleBidLanguages(final Document document) {
        return JsoupUtils.select("LANGUAGE LANGUAGE_OTHER, LANGUAGES LANGUAGE", document).stream()
            .map(n -> n.hasAttr("VALUE") ? n.attr("VALUE") : n.text()).collect(Collectors.toList());
    }

    /**
     * @param context
     *      context that includes body data
     * @return parsed body or null
     */
    private static ParsedBody parseBody(final Element context) {
        if (context == null) {
            return null;
        }

        Elements nodes = JsoupUtils.select("OFFICIALNAME, ORGANISATION", context);
        String name = null;
        if (nodes != null) {
            for (Element n : nodes) {
                if (n.children().isEmpty()) {
                    name = n.text();
                    break;
                }
            }
        }

        String contactPoint = JsoupUtils.selectText("ATTENTION", context);
        if(contactPoint == null){
            contactPoint = JsoupUtils.selectText("CONTACT_POINT", context);
        }
        
        return new ParsedBody()
            .setName(name)
            .addBodyId(new BodyIdentifier()
                .setId(JsoupUtils.selectText("NATIONALID", context))
                .setType(BodyIdentifier.Type.HEADER_ICO)
                .setScope(BodyIdentifier.Scope.NO))
            .setAddress(new ParsedAddress()
                .setStreet(JsoupUtils.selectText("ADDRESS", context))
                .setCity(JsoupUtils.selectText("TOWN", context))
                .setPostcode(JsoupUtils.selectText("POSTAL_CODE", context))
                .setCountry(JsoupUtils.selectAttribute("COUNTRY", "VALUE", context)))
            .setContactPoint(contactPoint)
            .setPhone(JsoupUtils.selectText("PHONE", context))
            .setEmail(JsoupUtils.selectText("E_MAIL", context));
    }

    /**
     * @param document
     *      parsed document
     * @param context
     *      context that includes buyer data
     * @return parsed buyer
     */
    private static ParsedBody parseBuyer(final Document document, final Element context) {
        ParsedBody buyer = parseBody(context);

        if (buyer == null) {
            buyer = new ParsedBody();
        }

        buyer.setBuyerType(JsoupUtils.selectAttribute("TYPE_OF_CONTRACTING_AUTHORITY, CA_TYPE",
                "VALUE", document))
            .addMainActivity(JsoupUtils.selectAttribute("TYPE_OF_ACTIVITY, CA_ACTIVITY, TYPE_OF_ACTIVITY_OTHER", "VALUE", document));

        if (buyer.getAddress() == null) {
            buyer.setAddress(new ParsedAddress());
        }
        buyer.getAddress().setUrl(JsoupUtils.selectText("URL_GENERAL", document));
        
        return buyer;
    }



    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "NO";
    }

    /**
     * Parse address of implementation.
     *
     * @param document document to parse from
     *
     * @return ParsedAddress or null
     */
    private ParsedAddress parseAddressOfImplementation(final Document document) {
        final ParsedAddress parsedAddress = new ParsedAddress()
                .setRawAddress(JsoupUtils.selectText("LOCATION_NUTS", document))
                .addNuts(JsoupUtils.selectAttribute("NUTS, SITE_OR_LOCATION NUTS", "CODE", document));

        return parsedAddress.getRawAddress() == null ? null : parsedAddress;
    }

    /**
     * Parse supply type.
     *
     * @param document document to parse from
     *
     * @return String or null
     */
    private String parseSupplyType(final Document document) {
        String supplyType = JsoupUtils.selectAttribute("TYPE_CONTRACT", "VALUE", document);

        if (supplyType == null) {
            supplyType = JsoupUtils.selectAttribute("TYPE_CONTRACT, FD_PRIOR_INFORMATION, FD_CONTRACT", "CTYPE", document);
        }

        return supplyType;
    }

    /**
     * Parse on behalf of.
     *
     * @param document document to parse from
     *
     * @return list of parsed bodies or null
     */
    private List<ParsedBody> parseOnBehalfOf(final Document document) {
        Elements nodes = JsoupUtils.select("PURCHASING_ON_BEHALF_YES > CONTACT_DATA_OTHER_BEHALF_CONTRACTING_AUTORITHY",
            document);

        if (nodes == null || nodes.isEmpty()) {
            return null;
        }

        return nodes.stream().map(n -> parseBody(n)).collect(Collectors.toList());
    }

    /**
     * Parse lots.
     *
     * @param document document to parse from
     *
     * @return List<ParsedTenderLot> or null
     */
    private List<ParsedTenderLot> parseLots(final Document document) {
        final Elements lots = JsoupUtils.select("AWARD_OF_CONTRACT_DEFENCE, OBJECT_DESCR, AWARD_CONTRACT,"
            + "AWARD_OF_CONTRACT", document);
        final List<ParsedTenderLot> parsedLots = new ArrayList<>();

        int positionOnPage = 1;
        for (Element n : lots) {
            ParsedTenderLot lot =  new ParsedTenderLot()
                .setPositionOnPage(String.valueOf(positionOnPage++))
                .setLotNumber(JsoupUtils.selectText("LOT_NUMBER, LOT_NO", n))
                .setContractNumber(JsoupUtils.selectText("CONTRACT_NUMBER, CONTRACT_NO", n))
                .setTitle(JsoupUtils.selectText("CONTRACT_TITLE, TITLE", n))
                .setBidsCount(JsoupUtils.selectText("OFFERS_RECEIVED_NUMBER, NB_TENDERS_RECEIVED", n))
                .setElectronicBidsCount(JsoupUtils.selectText("NB_TENDERS_RECEIVED_EMEANS", n))
                .addBid(new ParsedBid()
                    .addBidder(parseBody(n))
                    .setIsSubcontracted(parseBoolean("CONTRACT_LIKELY_SUB_CONTRACTED", n))
                    .setPrice(parseLotPrice(n))
                    .setIsWinning(Boolean.TRUE.toString()))
                .setAwardDecisionDate(parseDate("CONTRACT_AWARD_DATE, DATE_CONCLUSION_CONTRACT", n))
                .setCpvs(parseCPVs(n))
                .setAddressOfImplementation(new ParsedAddress()
                    .setState(JsoupUtils.selectAttribute("LOCATION", "code", n))
                    .setRawAddress(JsoupUtils.selectText("MAIN_SITE", n)))
                .addFunding(parseFunding(n))
                .setAreVariantsAccepted(parseBoolean("ACCEPTED_VARIANTS", n))
                .setHasOptions(parseBoolean("OPTIONS", n))
                .setDescription(JsoupUtils.selectText("SHORT_DESCR", n))
                .setSelectionMethod(JsoupUtils.exists("AC_PROCUREMENT_DOC", n, "MEAT", null));


            Element duration = JsoupUtils.selectFirst("DURATION", n);
            if (duration != null) {
                String type = duration.attr("TYPE");
                if (type != null) {
                    if (type.equals("DAY")) {
                        lot.setEstimatedDurationInDays(duration.text());
                    } else if (type.equals("MONTH")) {
                        lot.setEstimatedDurationInMonths(duration.text());
                    } else if (type.equals("YEAR")) {
                        lot.setEstimatedDurationInYears(duration.text());
                    }
                }
            }


            parsedLots.add(lot);
        }

        return parsedLots;
    }

    /**
     * @param lot
     *      lot to parse from
     * @return lot price or null
     */
    private ParsedPrice parseLotPrice(final Element lot) {
        Element price = JsoupUtils.selectFirst("VAL_TOTAL, INITIAL_ESTIMATED_TOTAL_VALUE_CONTRACT", lot);
        if (price == null) {
            return null;
        }

        return price.nodeName().equalsIgnoreCase("VAL_TOTAL") ? parseInlinePrice(price) : parsePrice(price);
    }

    /**
     * @param document
     *      document to parse from
     * @return tender final price or null
     */
    private ParsedPrice parseTenderFinalPrice(final Document document) {
        Element price = JsoupUtils.selectFirst("OBJECT_CONTRACT > VAL_TOTAL, COSTS_RANGE_AND_CURRENCY,"
            + "COSTS_RANGE_AND_CURRENCY_WITH_VAT_RATE", document);

        if (price == null) {
            return null;
        }

        return price.nodeName().equalsIgnoreCase("VAL_TOTAL") ? parseInlinePrice(price) : parsePrice(price);

    }
    
    /**
     * @param context
     *      context to parse from
     * @return parsed price or null
     */
    private ParsedPrice parsePrice(final Element context) {
        if (context == null) {
            return null;
        }

        ParsedPrice price = new ParsedPrice().setCurrency(context.attr("CURRENCY"));
        
        String value = JsoupUtils.selectText("VALUE_COST", context);
        if (JsoupUtils.exists("EXCLUDING_VAT", context)) {
            price.setNetAmount(value);
        } else {
            price.setAmountWithVat(value);
        }
        
        return price;
    }

    /**
     *
     * @param price
     *      price node to parse from
     * @return parsed price
     */
    private ParsedPrice parseInlinePrice(final Element price) {
        if (price == null) {
            return null;
        }
        
        return new ParsedPrice().setNetAmount(price.text()).setCurrency(price.attr("CURRENCY"));
    }

    /**
     * Parse date.
     *
     * @param selector selector to parse with
     * @param context context to parse from
     *
     * @return String or null
     */
    private String parseDate(final String selector, final Element context) {
        final Element dateElement = JsoupUtils.selectFirst(selector, context);

        if (dateElement == null) {
            return null;
        }

        final String day = JsoupUtils.selectText("DAY", dateElement);
        final String month = JsoupUtils.selectText("MONTH", dateElement);
        final String year = JsoupUtils.selectText("YEAR", dateElement);

        if (day == null || month == null || year == null) {
            return dateElement.text();
        } else {
            return day + "-" + month + "-" + year;
        }
    }

    /**
     * Parses documents location. It is the location accessible through folder icon on the result page.
     *
     * @param document parsed document
     *
     * @return documents location or null
     */
    private static ParsedAddress parseDocumentsLocation(final Document document) {
        // the URL is in "URL_DOCUMENT" element when we parse CFT, "EXTERNAL_DOCUMENT_URL" and
        // "EXPRESSION_OF_INTEREST_URL" are in CA.
        // "URL_INFORMATION" in CA represents different URL. See
        // https://www.doffin.no/Eps.Searching/UnsupportedNotice/NoticeXml/2016-665162 , where the element is filled,
        // but there is no documents location.
        String documentsUrl = JsoupUtils.selectText("URL_DOCUMENT, EXTERNAL_DOCUMENT_URL, EXPRESSION_OF_INTEREST_URL",
                document);

        if (documentsUrl == null) {
            return null;
        }

        return new ParsedAddress()
                .setUrl(documentsUrl);
    }

    /**
     * Parse publications.
     *
     * @param document document to parse from
     * @param machineReadableUrl machine readable url for included publication
     * @param publicationDate date of publishing of the tender
     * @return List<ParsedPublication> or null
     */
    private List<ParsedPublication> parsePublications(final Document document, final String machineReadableUrl,
        final String publicationDate) {
        final List<ParsedPublication> parsedPublications = new ArrayList<>();

        parsedPublications.add(new ParsedPublication()
            .setSource(PublicationSources.NO_DOFFIN)
            .setIsIncluded(true)
            .setSourceId(machineReadableUrl.substring(machineReadableUrl.lastIndexOf('/') + 1))
            .setMachineReadableUrl(machineReadableUrl)
            .setBuyerAssignedId(JsoupUtils.selectText("FILE_REFERENCE_NUMBER, REFERENCE_NUMBER", document))
            .setDispatchDate(parseDate("NOTICE_DISPATCH_DATE, DATE_DISPATCH_NOTICE", document))
            .setSourceFormType(JsoupUtils.selectAttribute("[form]", "form", document))
                .setVersion(JsoupUtils.selectAttribute("[version]", "version", document))
            .setPublicationDate(publicationDate));

        // previous TED publications
        Elements previousPublicationElements = JsoupUtils.select("CNT_NOTICE_INFORMATION", document);
        // publication can contain more links to previous TED publications (
        // https://www.doffin.no/en/Notice/Details/2012-246939 ). We use distinct, because there can be duplicates (
        // https://www.doffin.no/en/Notice/Details/2012-246729 )
        if (previousPublicationElements != null) {
            parsedPublications.addAll(previousPublicationElements.stream()
                .map(n -> {
                    return new ParsedPublication()
                        .setSource(PublicationSources.EU_TED)
                        .setIsIncluded(false)
                        .setSourceId(JsoupUtils.selectText("NOTICE_NUMBER_OJ", n))
                        .setPublicationDate(parseDate("DATE_OJ", n))
                        .setSourceFormType(JsoupUtils.selectAttribute("CNT_NOTICE_INFORMATION_S", "CHOICE", n));
                })
                .filter(ArrayUtils.distinct(n -> n.getSourceId()))
                .collect(Collectors.toList()));
        }


        // previous Doffin publications
        Elements previousPublicationDoffinIdElements = JsoupUtils.select("REFERENCE_DOFFIN", document);
        if (previousPublicationDoffinIdElements != null) {
        parsedPublications.addAll(previousPublicationDoffinIdElements
                .stream()
                .map(n -> {
                    return new ParsedPublication()
                        .setSource(PublicationSources.NO_DOFFIN)
                        .setIsIncluded(false)
                        .setSourceId(n.text())
                        .setPublicationDate(JsoupUtils.selectAttribute("DATE", n))
                        .setSourceFormType(JsoupUtils.selectAttribute("TYPE", n));
                })
                .filter(ArrayUtils.distinct(n -> n.getSourceId()))
                .collect(Collectors.toList()));
        }

        return parsedPublications;
    }

    @Override
    protected final List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }
}
