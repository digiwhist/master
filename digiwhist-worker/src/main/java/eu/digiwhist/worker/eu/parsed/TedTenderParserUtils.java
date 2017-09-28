package eu.digiwhist.worker.eu.parsed;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * Class provides set of functions for parsing typical xml fragments like address, subject etc.
 *
 * @author Tomas Mrazek
 */
final class TedTenderParserUtils {

    private static final Logger logger = LoggerFactory.getLogger(TedTenderParserUtils.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedTenderParserUtils() {
        throw new AssertionError();
    }

    /**
     * Parses datetime string from node in format Y-m-d H:i. Time part (H:i) is optional.
     *
     * @param dateTimeNode
     *         node with datetime data
     *
     * @return datetime string in format Y-m-d H:i
     */
    static String parseDateTime(final Element dateTimeNode) {
        if (dateTimeNode == null) {
            return null;
        }

        final String dateTime = JsoupUtils.selectText("YEAR", dateTimeNode, true) + "-" +
            JsoupUtils.selectText("MONTH", dateTimeNode, true) + "-" +
            JsoupUtils.selectText("DAY", dateTimeNode, true) + " " +
            JsoupUtils.selectText("TIME", dateTimeNode, true);

        return dateTime.trim();
    }

    /**
     * Parses address from node.
     *
     * @param addressNode
     *         node with address data
     *
     * @return parsed address
     */
    static ParsedAddress parseAddress(final Element addressNode) {
        if (addressNode == null) {
            return null;
        }

        return new ParsedAddress()
                .setStreet(JsoupUtils.selectText("ADDRESS", addressNode))
                .setCity(JsoupUtils.selectText("TOWN", addressNode))
                .setCountry(JsoupUtils.selectAttribute("COUNTRY", "VALUE", addressNode))
                .setPostcode(JsoupUtils.selectText("POSTAL_CODE", addressNode))
                .addNuts(JsoupUtils.selectAttribute("NUTS", "CODE", addressNode))
                .setUrl(JsoupUtils.selectText("URL_GENERAL, URL", addressNode));
    }

    /**
     * Parses price from node.
     *
     * @param priceNode
     *         node with price data
     *
     * @return parsed price
     */
    public static ParsedPrice parsePrice(final Element priceNode) {
        if (priceNode == null) {
            return null;
        }

        final ParsedPrice price = new ParsedPrice();
        price.setCurrency(JsoupUtils.selectAttribute("CURRENCY", priceNode));

        final String amount = JsoupUtils.selectAttribute("VALUE_COST", "FMTVAL", priceNode);
        final String minAmount = JsoupUtils.selectAttribute("RANGE_VALUE_COST > LOW_VALUE", "FMTVAL", priceNode);
        final String maxAmount = JsoupUtils.selectAttribute("RANGE_VALUE_COST > HIGH_VALUE", "FMTVAL", priceNode);

        if (!JsoupUtils.exists("INCLUDING_VAT", priceNode)) {
            price
                .setNetAmount(amount)
                .setMinNetAmount(minAmount)
                .setMaxNetAmount(maxAmount);
        } else {
            price
                .setAmountWithVat(amount)
                .setMinAmountWithVat(minAmount)
                .setMaxAmountWithVat(maxAmount)
                .setVat(JsoupUtils.selectAttribute("INCLUDING_VAT > VAT_PRCT", "FMTVAL", priceNode));
        }

        return price;
    }

    /**
     * Parses body from node.
     *
     * @param bodyNode
     *         node with body data
     *
     * @return parsed body
     */
    public static ParsedBody parseBody(final Element bodyNode) {
        if (bodyNode == null) {
            return null;
        }
        
        String name = JsoupUtils.selectText("OFFICIALNAME", bodyNode);
        if (name == null) {
            JsoupUtils.selectText("ORGANISATION", bodyNode);
        }

        final ParsedBody body = new ParsedBody()
            .setName(name)
            .setAddress(parseAddress(bodyNode))
            .setEmail(JsoupUtils.selectText("E_MAILS, E_MAIL", bodyNode))
            .setContactName(JsoupUtils.selectText("ATTENTION", bodyNode))
            .setContactPoint(JsoupUtils.selectText("CONTACT_POINT", bodyNode))
            .setPhone(JsoupUtils.selectText("PHONE", bodyNode));
        
        body.addBodyId(parseBodyIdentifier(JsoupUtils.selectFirst("NATIONALID", bodyNode), body.getAddress()));

        return body;
    }

    /**
     * Parses body indetifier.
     *
     * @param idNode
     *      node with identifier code
     * @param address
     *      body address, includes country which is used as default scope
     * @return body indetifier or null
     */
    public static BodyIdentifier parseBodyIdentifier(final Element idNode, final ParsedAddress address) {
        if (idNode == null) {
            return null;
        }
        
        BodyIdentifier.Scope scope = BodyIdentifier.Scope.UNKNOWN;
        if (address != null && address.getCountry() != null) {
            try {
                String country = address.getCountry();
                // valid scope code for United Kingdom is GB
                country = country.replace("UK", "GB");

                scope = BodyIdentifier.Scope.valueOf(country);
            } catch (IllegalArgumentException ex) {
                logger.error("Unable to parse body identifier scope from address country '{}', set to UNKNOWN",
                    address.getCountry());
            }
        } else {
            logger.error("Unable to parse body identifier scope because of an empty address country, set to UNKNOWN");
        }

        return new BodyIdentifier()
            .setId(idNode.text())
            .setType(BodyIdentifier.Type.ORGANIZATION_ID)
            .setScope(scope);
    }

    /**
     * Parses CPVs from node.
     *
     * @param cpvsNode
     *         node with CPVs data
     *
     * @return list of parsed CPVs or null if list is empty
     */
    public static List<ParsedCPV> parseCpvs(final Element cpvsNode) {
        final Elements cpvsNodes = JsoupUtils.select("CPV_MAIN, CPV_ADDITIONAL", cpvsNode);
        if (cpvsNodes == null || cpvsNodes.isEmpty()) {
            return null;
        }

        final List<ParsedCPV> cpvs = new ArrayList();
        for (final Element node : cpvsNodes) {
            cpvs.add(new ParsedCPV()
                .setCode(JsoupUtils.selectAttribute("CPV_CODE", "CODE", node))
                .setIsMain(Boolean.toString(node.tagName().equalsIgnoreCase("CPV_MAIN")))
            );
        }

        return cpvs;
    }

    /**
     * Parses funding.
     *
     * @param fundingNode
     *         node with funding data
     *
     * @return parsed funding or null
     */
    public static ParsedFunding parseFunding(final Element fundingNode) {
        if (fundingNode == null) {
            return null;
        }

        return new ParsedFunding()
            .setIsEuFund(JsoupUtils.exists("RELATES_TO_EU_PROJECT_YES", fundingNode).toString())
            .setProgramme(JsoupUtils.selectText("RELATES_TO_EU_PROJECT_YES", fundingNode));
    }

    /**
     * Returns origin node for parsing.
     *
     * @param document
     *      parsed document
     * @return origin node
     */
    public static Element getOriginNode(final Document document) {
        final Element originNode =
            JsoupUtils.selectFirst("TED_EXPORT > FORM_SECTION > *[CATEGORY=ORIGINAL] > *", document);

        if (originNode == null) {
            throw new UnrecoverableException("Unrecognized document structure");
        }

        return originNode;
    }

    /**
     * Returns node of coded data section.
     *
     * @param document
     *          parsed document
     * @return origin node
     */
    public static Element getCodedDataNode(final Document document) {
        return JsoupUtils.selectFirst("TED_EXPORT > CODED_DATA_SECTION", document);
    }

    /**
     * Returns node of translation section.
     *
     * @param document
     *          parsed document
     * @return origin node
     */
    public static Element getTranslationNode(final Document document) {
        return JsoupUtils.selectFirst("TED_EXPORT > TRANSLATION_SECTION", document);
    }

    /**
     * Prases tender procedure type.
     *
     * @param procedureNode
     *      node with procedure data
     * @return procedure type
     */
    public static String parseProcedureType(final Element procedureNode) {
        final Element procedureTypeNode = JsoupUtils.selectFirst("> *", procedureNode);
        if (procedureTypeNode == null) {
            return null;
        }

        if (procedureTypeNode.hasAttr("VALUE")) {
            return procedureTypeNode.attr("VALUE");
        } else {
            return procedureTypeNode.tagName();
        }
    }

    /**
     * Parses npwp reasons.
     *
     * @param context
     *      context that includes npwp reasons nodes
     * @return list of npwp reasons
     */
    public static List<String> parseNpwpReasons(final Element context) {
        Elements npwpReasonNodes = JsoupUtils.select("JUSTIFICATION_CHOICE_NEGOCIATED_PROCEDURE *:empty,"
            + " ANNEX_D *:empty, JUSTIFICATION_CHOICE_NEGOCIATED_PROCEDURE REASON_CONTRACT_LAWFUL,"
            + " ANNEX_D REASON_CONTRACT_LAWFUL", context);

        if (npwpReasonNodes == null || npwpReasonNodes.isEmpty()) {
            return null;
        }

        final List<String> reasons = new ArrayList<>();
        npwpReasonNodes.forEach(n -> {
            // negations of the npwp reasons are skipped
            if (n.nodeName().toUpperCase().startsWith("NO_")) {
                return;
            }
            reasons.add(n.nodeName().equalsIgnoreCase("REASON_CONTRACT_LAWFUL") ? n.text() : n.nodeName());
        });

        return reasons.isEmpty() ? null : reasons;
    }

    /**
     * Parses address of implementation.
     *
     * @param contractNode
     *      contract node
     * @return address of implementation
     */
    public static ParsedAddress parseAddressOfImplementation(final Element contractNode) {
        String nuts = JsoupUtils.selectAttribute("LOCATION_NUTS > NUTS", "CODE", contractNode);
        if (nuts == null) {
            nuts = getDefaultNuts(contractNode.ownerDocument());
        }

        return new ParsedAddress()
            .addNuts(nuts)
            .setRawAddress(JsoupUtils.selectText("LOCATION_NUTS > LOCATION", contractNode));
    }

    /**
     * Parses list of main activities.
     *
     * @param activityNodes
     *      activities nodes
     * @param doc
     *      parsed document
     * @return list of activities
     */
    public static List<String> parseBuyerMainActivities(final Elements activityNodes, final Document doc) {
        final List<String> activities = new ArrayList<>(getDefaultMainActivities(doc));
        if (activities.isEmpty() && activityNodes != null) {
            activityNodes.forEach((node) -> {
                activities.add(node.hasAttr("VALUE") ? node.attr("VALUE") : node.text());
            });
        }

        return activities.isEmpty() ? null : activities;
    }

    /**
     * Parses list of bodies.
     *
     * @param bodiesNodes
     *      nodes of bodies
     * @return list of bodies
     */
    public static List<ParsedBody> parseBodies(final Elements bodiesNodes) {
        if (bodiesNodes == null || bodiesNodes.isEmpty()) {
            return null;
        }

        final List<ParsedBody> bodies = new ArrayList<>();
        for (Element node : bodiesNodes) {
            bodies.add(TedTenderParserUtils.parseBody(node));
        }

        return bodies;
    }

    /**
     * Parses award criteria.
     *
     * @param criteriaNode
     *         node with criteria data
     *
     * @return list of parsed award criteria
     */
    public static List<ParsedAwardCriterion> parseAwardCritera(final Element criteriaNode) {
       final Elements criteriaNodes = JsoupUtils.select(
                "MOST_ECONOMICALLY_ADVANTAGEOUS_TENDER_SHORT > CRITERIA_DEFINITION,"
                    + " MOST_ECONOMICALLY_ADVANTAGEOUS_TENDER > CRITERIA_STATED_BELOW > CRITERIA_DEFINITION,"
                    + " LOWEST_PRICE", criteriaNode);

        if (criteriaNodes == null || criteriaNodes.isEmpty()) {
            return null;
        }

        final List<ParsedAwardCriterion> awardCriteria = new ArrayList<>();
        for (Element node : criteriaNodes) {
            final ParsedAwardCriterion criterion = new ParsedAwardCriterion();
            if (node.tagName().equalsIgnoreCase("LOWEST_PRICE")) {
                criterion
                    .setName(node.tagName().toUpperCase())
                    .setIsPriceRelated(Boolean.TRUE.toString());
            } else {
                criterion
                    .setName(JsoupUtils.selectText("CRITERIA", node))
                    .setWeight(JsoupUtils.selectText("WEIGHTING", node));
            }

            awardCriteria.add(criterion);
        }

        return awardCriteria;
    }

    /**
     * Parses the selection method.
     *
     * @param codedDataNode
     *      coded data node
     * @return selection method name
     */
    public static String parseSelectionMethod(final Element codedDataNode) {
        return JsoupUtils.selectText("CODIF_DATA > AC_AWARD_CRIT", codedDataNode);
    }

    /**
     * @param doc
     *      parsed document
     * @return default NUTS code of the document
     */
    public static String getDefaultNuts(final Document doc) {
        return JsoupUtils.selectAttribute("NOTICE_DATA > ORIGINAL_NUTS", "CODE", getCodedDataNode(doc));
    }

    /**
     * @param doc
     *      parsed document
     * @return default buyer type
     */
    public static String getDefaultBuyerType(final Document doc) {
        String type = JsoupUtils.selectText("CODIF_DATA > AA_AUTHORITY_TYPE", getCodedDataNode(doc));
        return isNull().test(type) ? null : type;
    }

    /**
     * @param doc
     *      parsed document
     * @return default main activities list or empty list
     */
    public static List<String> getDefaultMainActivities(final Document doc) {
        Elements nodes = JsoupUtils.select("CODIF_DATA > MA_MAIN_ACTIVITIES", getCodedDataNode(doc));
        if (nodes == null) {
            return Collections.emptyList();
        }
        
        return nodes.stream()
            .map(n -> n.ownText())
            .filter(isNotNull())
            .collect(Collectors.toList());
    }

    /**
     * @return predicator which tests if the string is null
     */
    public static Predicate<String> isNull() {
        return (final String t) -> t == null || t.matches("(?i)Not applicable|specified");
    }

    /**
     * @return predicator which tests if the string is not null
     */
    public static Predicate<String> isNotNull() {
        return isNull().negate();
    }

    /**
     * Parses buyer.
     *
     * @param buyerNode
     *         node with buyer data
     *
     * @return parsed buyer
     */
    public static ParsedBody parseBuyer(final Element buyerNode) {
        if (buyerNode == null) {
            return null;
        }

        final Element typeAndActivitiesNode = JsoupUtils.selectFirst(
                "TYPE_AND_ACTIVITIES_AND_PURCHASING_ON_BEHALF > TYPE_AND_ACTIVITIES", buyerNode);

        String buyerType = getDefaultBuyerType(buyerNode.ownerDocument());
        if (buyerType == null) {
            buyerType = JsoupUtils.selectAttribute("TYPE_OF_CONTRACTING_AUTHORITY, TYPE_OF_CONTRACTING_AUTHORITY_OTHER",
                "VALUE", typeAndActivitiesNode);
        }

        return TedTenderParserUtils.parseBody(JsoupUtils.selectFirst("NAME_ADDRESSES_CONTACT_CONTRACT,"
            + " NAME_ADDRESSES_CONTACT_CONTRACT_AWARD", buyerNode))
                .setMainActivities(TedTenderParserUtils.parseBuyerMainActivities(JsoupUtils.select("TYPE_OF_ACTIVITY,"
                    + " TYPE_OF_ACTIVITY_OTHER", typeAndActivitiesNode), buyerNode.ownerDocument()))
                .setBuyerType(buyerType);
    }

    /**
     * Parses main tender publication.
     *
     * @param doc
     *      parsed document
     * @return main publication with set of base fields
     */
    public static ParsedPublication initMainPublication(final Document doc) {
        Element codedDataNode = getCodedDataNode(doc);

        return new ParsedPublication()
            .setLanguage(JsoupUtils.selectText("NOTICE_DATA > LG_ORIG", codedDataNode))
            .setHumanReadableUrl(JsoupUtils.selectText("NOTICE_DATA > URI_LIST > URI_DOC[LG=EN]", codedDataNode))
            .setSourceFormType(getMainPublicationSourceFormType(doc))
            .setSourceId(JsoupUtils.selectText("NOTICE_DATA > NO_DOC_OJS", codedDataNode))
            .setIsIncluded(true)
            .setPublicationDate(JsoupUtils.selectText("REF_OJS > DATE_PUB", codedDataNode));
    }

    /**
     * Gets type of form for parsed document. Codes corresponding to the list on
     * http://simap.ted.europa.eu/web/simap/standard-forms-for-public-procurement.
     *
     * @param doc
     *         parsed document
     * @return type of form
     */
    public static TedFormType getFormType(final Document doc) {
        String rawFormType = JsoupUtils.selectAttribute("TED_EXPORT > FORM_SECTION > *", "FORM", doc);
        
        // corrigendum
        if (rawFormType == null
            && JsoupUtils.hasText("TD_DOCUMENT_TYPE", getCodedDataNode(doc), "Additional information")) {
            rawFormType = "14";
        }

        if (rawFormType == null || rawFormType.isEmpty()) {
            logger.error("Unable to get the document's form type");
            throw new UnrecoverableException("Unable to get the document's form type");
        }

        final TedFormType formType = TedFormType.getFormTypeFromCode(rawFormType);
        if (formType == null) {
            logger.error("Unable to get the document's form type from code '{}'.", rawFormType);
            throw new UnrecoverableException("Unable to get the document's form type");
        }

        return formType;
    }


    /**
     * Gets version of the form.
     *
     * @param doc
     *         parsed document
     * @return version of the form
     */
    public static TedFormVersionType getFormVersion(final Document doc) {
        String version = JsoupUtils.selectAttribute("TED_EXPORT", "VERSION", doc);
        if (version == null) {
            version = JsoupUtils.selectAttribute("TED_EXPORT > FORM_SECTION > *", "VERSION", doc);
        }

        if (version == null) {
            logger.error("Unable to get document's version");
            throw new UnrecoverableException("Unable to get document's version");
        }

        if (TedFormVersionType.R209.is(version, TedFormVersionType.IS_YOUNGER)) {
            return TedFormVersionType.PRIOR_R209;
        }

        return TedFormVersionType.R209;
    }

    /**
     * From the form type and version compose publication source form type.
     *
     * @see #getFormType(org.jsoup.nodes.Document)
     * @see #getFormVersion(org.jsoup.nodes.Document)
     *
     * @param doc
     *      parsed document
     * @return source form type
     */
    public static String getMainPublicationSourceFormType(final Document doc) {
        TedFormType type = getFormType(doc);
        TedFormVersionType version = getFormVersion(doc);

        return String.format("F%02d", type.getCode()) + "_"
            + (version.is(TedFormVersionType.R209, TedFormVersionType.IS_OLDER) ? "2011" : "2014");
    }
}
