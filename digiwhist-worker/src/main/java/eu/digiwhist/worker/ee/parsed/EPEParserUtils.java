package eu.digiwhist.worker.ee.parsed;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.TenderSize;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.StringUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * Class provides useful functions for EPE parsing.
 *
 * @author Tomas Mrazek
 */
public final class EPEParserUtils {

    private static final Logger logger = LoggerFactory.getLogger(EPEParserUtils.class);

    /**
     * Supress default constructor for noninstantiability.
     */
    private EPEParserUtils() {
        throw new AssertionError();
    }

    /**
     * In the given {@code context} attempts to find an tr element that includes the {@code label}. If such element
     * exists, applies the {@code regex} and returns value. Method assumes that regex includes group named "value"
     * ({@code (?<value>...)} - {@link Pattern}).
     *
     * @param label
     *      wanted label
     * @param regex
     *      value regular expression
     * @param context
     *      context
     * @return value or null
     */
    public static String regexValueByLabel(final String label, final String regex, final Element context) {
        Element node = JsoupUtils.selectFirst("tr:matches(" + label + ")", context);
        if (node == null) {
            return null;
        }
        Matcher m = Pattern.compile(regex).matcher(node.text());
        return m.find() ? m.group("value") : null;
    }

    /**
     * @param label
     *      label
     * @param context
     *      context that includes award criteria data
     * @return non-empty list of criteria or null
     */
    public static List<ParsedAwardCriterion> parseAwardCriteria(final String label, final Element context) {
        String selectionMethod = EPEParserUtils.tableValueByLabel(label, context);
        if (selectionMethod == null) {
            return null;
        }
        
        assert selectionMethod.equals("Madalaim hind") || selectionMethod.equals("Parima hinna ja kvaliteedi suhe");

        List<ParsedAwardCriterion> criteria = new ArrayList<>();
        if (selectionMethod.equals("Madalaim hind")) {
            criteria.add(new ParsedAwardCriterion().setName(selectionMethod));
        } else {
            Element node = context.select("tr:matches(" + label + ") + tr + tr > td:eq(1)").first();
            if (node != null) {
                // data spliterator for easier matching
                String data = node.html().replace("<br>", "~");
                Matcher m = Pattern.compile("(Kirjeldus: (?<name>[^~]+)~ Osakaal: (?<weight>[^~]+))").matcher(data);
                while (m.find()) {
                    criteria.add(new ParsedAwardCriterion().setName(m.group("name")).setWeight(m.group("weight")));
                }
            }
        }
        return criteria.isEmpty() ? null : criteria;
    }

    /**
     * @param context
     *      context that includes buyer data
     * @return parsed buyer or null
     */
    public static ParsedBody parseBuyer(final Element context) {
        ParsedBody buyer = parseBody("^I\\.1\\)", context);
        if (buyer == null) {
            return null;
        }

        Element activitiesNode = tableValueNodeByLabel("HANKIJA PÕHITEGEVUS", context);
        List<String> activities = activitiesNode != null ? StringUtils.split(activitiesNode.html(), "<br>") : null;
        
        return buyer
            .setMainActivities(activities == null || activities.isEmpty() ? null : activities)
            .setBuyerType(tableValueByLabel("HANKIJA LIIK", context));
    }

    /**
     * Returns value with the given {@code label}.
     *
     * <pre>
     * {@code
     *      <tr>...#label#...</tr>
     *      <tr><td></td><td>#value#</td>...</tr>
     * }
     * </pre>
     *
     * @param label
     *      label
     * @param context
     *      context that includes labeled value
     * @return value or null
     */
    public static String tableValueByLabel(final String label, final Element context) {
        Element node = tableValueNodeByLabel(label, context);        
        return node == null ? null : node.text();
    }

    /**
     * Returns node that includes value for the given {@code label}.
     *
     * <pre>
     * {@code
     *      <tr>...#label#...</tr>
     *      <tr><td></td><td>#value#</td>...</tr>
     * }
     * </pre>
     *
     * @param label
     *      label
     * @param context
     *      context that includes labeled value node
     * @return node or null
     */
    public static Element tableValueNodeByLabel(final String label, final Element context) {
        return JsoupUtils.selectFirst("tr:matches(" + label + ") + tr > td:eq(1)", context);
    }

    /**
     * @param label
     *      label
     * @param context
     *      context that includes CPVs
     * @return non-empty list of CVPs or null
     */
    public static List<ParsedCPV> parseCPVs(final String label, final Element context) {
        String codes = regexValueByLabel(label, "\\(CPV\\) (?<value>.+)", context);
        if (codes == null) {
            return null;
        }
        List<ParsedCPV> cpvs = new ArrayList<>();
        Matcher m = Pattern.compile("\\d{8}(-\\d+)?").matcher(codes);
        while (m.find()) {
            cpvs.add(new ParsedCPV().setCode(m.group()));
        }
        return cpvs.isEmpty() ? null : cpvs;
    }

    /**
     * Parses lots starting at the given {@code firstRow}. In very first extracts lots as independent tables
     * ({@link EPEParserUtils#parseRepeatedParts(org.jsoup.nodes.Element, org.jsoup.nodes.Element)}), then applies
     * {@code parser} on each element.
     *
     * @param parser
     *      lot parser
     * @param firstRow
     *      row where the lots data starts     
     * @param lastRow
     *      row where lots parsing ends, in case it is null parsing ends at the end of table
     * @return non-empty list of lots or null
     */
    public static List<ParsedTenderLot> parseLots(final Function<Element, ParsedTenderLot> parser,
        final Element firstRow, final Element lastRow) {
        
        final List<Element> lotNodes = parseRepeatedParts(firstRow, lastRow, "(?i)(OSA|LEPING) d+");
        if (lotNodes == null) {
            return null;
        }
        
        return lotNodes.stream().map(parser).collect(Collectors.toList());
    }

    /**
     * Parses repeated parts starting at the given {@code firstRow} and ending on {@code lastRow}. For each part is made
     * an independent table. The starting node, repectively endig node of the previous part, includes the given
     * {@code label}.
     *
     * @param firstRow
     *      row where the lots data starts
     * @param lastRow
     *      the first row after the lot data. In case it is null parsing ends at the end of table otherwise, ends on
     *      this row. This row is not included in the last table.
     * @param partLabelRegex
     *      the regex of the part label
     * @return non-empty list of lot nodes or null
     */
    public static List<Element> parseRepeatedParts(final Element firstRow, final Element lastRow,
        final String partLabelRegex) {

        final List<Element> lotNodes = new ArrayList<>();

        String lotHtml = "";
        Element table = Jsoup.parse("<table></tbale>").select("table").first();     
        
        Element row = firstRow;
        while (row != null && (lastRow == null || row.siblingIndex() < lastRow.siblingIndex())) {
            // find start of the lot
            if (row.text().matches(partLabelRegex)) {
                if (!lotHtml.isEmpty()) {
                    lotNodes.add(table.clone().append(lotHtml));
                }
                lotHtml = "";
            }
            // append row into the  lot html
            lotHtml += row.outerHtml();
            // move on the next row
            row = row.nextElementSibling();
        }

        if (!lotHtml.isEmpty()) {
            lotNodes.add(table.clone().append(lotHtml));
        }

        return lotNodes.isEmpty() ? null : lotNodes;
    }

    /**
     * @param label
     *      label of the boolean value
     * @param context
     *      context
     * @return "true" only and only if the value ends with "jah" (ignoring case), otherwise "false"
     */
    public static String parseBoolean(final String label, final Element context) {
        String bool = regexValueByLabel(label, "(?i)(?<value>jah|(n?ei))$", context);
        return parseBoolean(bool).toString();
    }

    /**
     * @param input
     *      input string
     * @return "true" only and only if the input string ends with "jah" (ignoring case), otherwise "false"
     */
    public static Boolean parseBoolean(final String input) {
        return (input != null && input.equalsIgnoreCase("jah") ? Boolean.TRUE : Boolean.FALSE);
    }

    /**
     * @param label
     *      label
     * @param context
     *      context that includes body data
     * @return parsed body or null
     */
    public static ParsedBody parseBody(final String label, final Element context) {
        String data = tableValueByLabel(label, context);
        if (data == null) {
            return null;
        }

        Matcher m = Pattern.compile(
            "(?<name>[^,]+)(?:, (?<id>\\d+))?(?:, (?<street>[^,]+))?(?:, (?<city>[^,]+))?(?:, [^,\\d]+)*"
            + "(?:, (?<zip>\\d+))? (?<country>[^~]+)(?:~\\((?<iso>[A-Z]{2})\\))?"
            + "(~Kontaktisik: (?<contact>[^~]+))?"
            + "(~Tel\\.: (?<phone>[^~]+))?"
            + "(?:~Faks: [^~]+)?"
            + "(~E\\-post: (?<email>[^~]+))?"
            + "(~URL: (?<url>[^~]+))?"
            + "(?:~Hankijaprofiili aadress: [^~]+)?")
            // append data separator '~' for easier matching
            .matcher(data.replaceAll(" (\\([A-Z]{2}\\)|((Kontaktisik|Tel\\.|Faks|E\\-post|URL|Hankijaprofiili"
                + " aadress):))", "~$1"));

        if (!m.find()) {
            logger.error("Unable to parse body because of unexpected data structure");
            throw new UnrecoverableException("Unable to parse body");
        }

        ParsedBody body = new ParsedBody().setName(m.group("name"))            
            .setAddress(new ParsedAddress().setCountry(m.group("iso"))
                .setCity(m.group("city")).setStreet(m.group("street")).setPostcode(m.group("zip"))
                .setUrl(m.group("url")))
            .setContactName(m.group("contact"))
            .setPhone(m.group("phone"))
            .setEmail(m.group("email"));

        if (m.group("id") != null) {
            body.addBodyId(
                // BodyIdentifier.Type.TRADE_REGISTER ???
                new BodyIdentifier().setId(m.group("id"))
                    .setType(BodyIdentifier.Type.TRADE_REGISTER)
                    .setScope(BodyIdentifier.Scope.EE));
        }
        
        return body;
    }

    /**
     * Parses tender size.
     *
     * @param context
     *      context
     * @return "BELOW_THE_THRESHOLD" or null
     */
    public static String parseTenderSize(final Element context) {
        String aboveTheThreshold = EPEParserUtils.parseBoolean("Kas riigihanke eeldatav maksumus on võrdne"
            + " rahvusvahelise piirmääraga või ületab seda", context);
        return aboveTheThreshold.equals(Boolean.FALSE.toString()) ? TenderSize.BELOW_THE_THRESHOLD.name() : null;
    }

    /**
     * @param context
     *      context
     * @return "true" only and only if appropriate node includes string "Raamlepingu sõlmimiseks", otherwise "false"
     */
    public static String parseIsFrameworkAgreement(final Element context) {
        String value = EPEParserUtils.tableValueByLabel("^II\\.1\\.3\\)", context);

        return (value != null && value.equalsIgnoreCase("Raamlepingu sõlmimiseks") ? Boolean.TRUE : Boolean.FALSE)
            .toString();
    }

    /**
     * @param context
     *      context that includes address of implementation data
     * @return address or null
     */
    public static ParsedAddress parseAddressOfImplementation(final Element context) {
        String rawAddress = EPEParserUtils.regexValueByLabel(
            "Ehitustööde teostamise koht|Asjade tarnekoht|Teenuse osutamise koht", ": (?<value>.+)", context);
        
        String nuts = EPEParserUtils.regexValueByLabel("NUTS kood", ": (?<value>.+)", context);

        if (rawAddress == null && nuts == null) {
            return null;
        }

        return new ParsedAddress().setRawAddress(rawAddress).addNuts(nuts);
    }

    /**
     * @param label
     *      label
     * @param context
     *      context that includes price data
     * @return price or null
     */
    public static ParsedPrice parsePrice(final String label, final Element context) {
        // origin node for price prasing
        Element node = JsoupUtils.selectFirst("tr:matches(" + label + ") + tr", context);
        if (node == null) {
            return null;
        }
        // find node with currency (it is always listed) unlike the price
        while (node != null && node.siblingIndex() <= node.lastElementSibling().siblingIndex()) {
            if (node.text().contains("Rahaühik")) {
                // in case the price is listed is placed in the node that precedes the currency node and its label
                // includes string 'maksumus' (case insesitively)
                Element priceNode = node.previousElementSibling();
                if (priceNode != null && priceNode.text().matches("(?i).*maksumus.*")) {
                    return new ParsedPrice()
                        .setNetAmount(priceNode.text().replaceAll(".+: ([\\d,]+)", "$1"))
                        .setCurrency(parseCurrency(node.text()));
                }
                break;
            }

            node = node.nextElementSibling();
        }

        return null;
    }

    /**
     * Attempts to fix the given {@code currency} string.
     *
     * @param currency
     *      currency string
     * @return fixed currency string
     */
    public static String parseCurrency(final String currency) {
        if (currency == null) {
            return null;
        }
        
        return currency
            .replace("Rahaühik: ", "")
            .replace("Euro", "EUR");
    }

    /**
     * @param label
     *      label
     * @param context
     *      context that includes appeal body data
     * @return appeal body name or null
     */
    public static String parseAppealBodyName(final String label, final Element context) {
        String data = tableValueByLabel(label, context);
        if (data == null) {
            return null;
        }

        Matcher m = Pattern.compile("(?<name>[^,]+).*").matcher(data);
        return m.find() ? m.group("name") : null;
    }

    /**
     * @param label
     *      label
     * @param context
     *      context taht includes datetime
     * @return date(time) or null
     */
    public static String parseDateTime(final String label, final Element context) {
        return EPEParserUtils.regexValueByLabel(label, "(?<value>\\d{2}.\\d{2}.\\d{4}( \\d{2}:\\d{2})?)", context);
    }

    /**
     * Parses EU funding if exists.
     *
     * @param label
     *      label
     * @param context
     *      context that includes EU funding data
     * @return EU funding or null
     */
    public static ParsedFunding parseEUFunding(final String label, final Element context) {
        Element node = tableValueNodeByLabel(label, context);
        if (node != null) {
            // if true, the following row includes program name
            if (parseBoolean(node.text())) {
                return new ParsedFunding()
                    .setIsEuFund(Boolean.TRUE.toString())
                    .setProgramme(node.nextElementSibling().text());
            } else {
                return new ParsedFunding().setIsEuFund(Boolean.FALSE.toString());
            }
        }

        return null;
    }

    /**
     * Parses part of the form which starts at the row with the given {@code label}. Form part is extracted as
     * independent table.
     *
     * @param label
     *      the form part label
     * @param context
     *      context that includes form data
     * @return form part or null
     */
    public static Element parseFormPart(final String label, final Element context) {
        String lotHtml = "";
        Element part = Jsoup.parse("<table></tbale>").select("table").first();

        // the first row of a form part is the row that inmediately follows after the row with the given label
        Element row = JsoupUtils.selectFirst("tr:matches(" + label + ") + tr", context);

        while (row != null) {
            // find start of the next form part, each part starts with label in specific form
            if (!lotHtml.isEmpty() && row.text().matches("[A-Z]{1,2} (OSA|LISA):.+")) {
                return part.clone().append(lotHtml);
            }
            // append row into the form part html
            lotHtml += row.outerHtml();
            // move on the next row
            row = row.nextElementSibling();
        }
        
        return !lotHtml.isEmpty() ? part.clone().append(lotHtml) : null;
    }

    /**
     * @param doc
     *      parsed document
     * @return parsed tender with included publication and title
     */
    public static ParsedTender parsePublicationAndTitle(final Document doc) {
        Element dataTable = getDataTable(doc);

        return new ParsedTender()
            .addPublication(new ParsedPublication()
                .setIsIncluded(true)
                .setSource(PublicationSources.EE_EPE)
                .setSourceFormType(JsoupUtils.selectText("tr:eq(0)", dataTable))
                .setSourceTenderId(EPEParserUtils.regexValueByLabel("Hanke viitenumber", "(?<value>\\d+)", dataTable))
                .setHumanReadableUrl(JsoupUtils.selectAttribute("#GenericLink", "href", doc))
                .setPublicationDate(EPEParserUtils.regexValueByLabel("Teate avaldamise kuupäev", "(?<value>\\d.*)",
                        dataTable)))
            .setTitle(EPEParserUtils.regexValueByLabel("Hanke nimetus", "nimetus (?<value>.+)", dataTable));
    }

    /**
     * @param doc
     *      parsed document
     * @return origin table element that includes all data (except permalink)
     */
    public static Element getDataTable(final Document doc) {
        return JsoupUtils.selectFirst("#deftbl > table", doc);
    }

    /**
     * Parses common data for contract notice and contract award.
     *
     * @param doc
     *      parsed document
     * @return parsed tender with common fields
     */
    public static ParsedTender parseNoticeAwardCommonData(final Document doc) {
        Element dataTable = EPEParserUtils.getDataTable(doc);

        String procedureType = EPEParserUtils.tableValueByLabel("^IV\\.1\\.1\\)", dataTable);

        return EPEParserUtils.parsePublicationAndTitle(doc)
            .setSize(EPEParserUtils.parseTenderSize(dataTable))
            .addBuyer(EPEParserUtils.parseBuyer(dataTable))
            .setIsOnBehalfOf(EPEParserUtils.parseBoolean("Hankija teostab hanget teiste hankijate nimel", dataTable))
            .setAddressOfImplementation(EPEParserUtils.parseAddressOfImplementation(dataTable))
            .setIsFrameworkAgreement(EPEParserUtils.parseIsFrameworkAgreement(dataTable))
            .setNationalProcedureType(procedureType)
            .setProcedureType(procedureType)
            .setSelectionMethod(EPEParserUtils.tableValueByLabel("^IV\\.2\\.1\\)", dataTable))
            .setAwardCriteria(EPEParserUtils.parseAwardCriteria("^IV\\.2\\.1\\)", dataTable))
            .setIsElectronicAuction(EPEParserUtils.parseBoolean("^IV\\.2\\.2\\)", dataTable));
    }
}
