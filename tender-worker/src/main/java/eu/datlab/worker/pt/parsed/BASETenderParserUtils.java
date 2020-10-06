package eu.datlab.worker.pt.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Useful functions for BASE parsers.
 * 
 * @author Tomas Mrazek
 */
public final class BASETenderParserUtils {
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy/MM/dd");


    /**
     * Supress default constructor for non-instatiability.
     */
    private BASETenderParserUtils() {
    }

    /**
     * Parses body from the given {@code bodyNode}.
     *
     * @param bodyNode
     *      node that includes body data
     * @return parsed body or null
     */
    public static ParsedBody parseBody(final Element bodyNode) {
        return parseBodyFromAnchor(JsoupUtils.selectFirst("a", bodyNode));
    }

    /**
     * Parses all bodies from the given {@code bodiesNode}.
     *
     * @param bodiesNode
     *      node that includes bodies data
     * @return non-empty list of parsed bodies or null
     */
    public static List<ParsedBody> parseBodies(final Element bodiesNode) {
        Elements bodyNodes = JsoupUtils.select("a", bodiesNode);
        if (bodyNodes == null || bodyNodes.isEmpty()) {
            return null;
        }

        List<ParsedBody> bodies = new ArrayList<>();
        bodyNodes.stream().forEach(n -> {
            bodies.add(parseBodyFromAnchor(n));
        });

        return bodies;
    }

    /**
     * Parses body from HTML anchor.
     *
     * @param anchor
     *      anchor that inlcudes body data
     * @return parsed body or null
     */
    public static ParsedBody parseBodyFromAnchor(final Element anchor) {
        if (anchor == null) {
            return null;
        }

        ParsedBody body = new ParsedBody()
            .setAddress(new ParsedAddress().setUrl(JsoupUtils.selectAttribute("href", anchor)));

        Matcher m = Pattern.compile("(.*)\\(([^\\)]+)\\)").matcher(anchor.text());
        if (m.find()) {
            body.addBodyId(new BodyIdentifier()
                    .setId(m.group(2))
                    .setScope(BodyIdentifier.Scope.PT)
                    .setType(BodyIdentifier.Type.ORGANIZATION_ID));

            final String trimmedName = m.group(1).trim();
            boolean isDate;
            try {
                DATE_FORMATTER.parse(trimmedName);
                isDate = true;
            } catch (DateTimeParseException ex) {
                isDate = false;
            }

            if (!(
                    trimmedName.equals(
                            "(No caso de se tratar de um agrupamento, separar os sucessivos nomes e os sucessivos NIF por ponto e vírgula)")
                    || isNIF(trimmedName)
                    || isDate
            )) {
                body.setName(m.group(1));
            }
        }

        return body;
    }

    /**
     * Parses price from the given {@code priceNode}.
     *
     * @param priceNode
     *      node that includes price data
     * @return parsed price or null
     */
    public static ParsedPrice parsePrice(final Element priceNode) {
        if (priceNode == null) {
            return null;
        }

        Matcher m = Pattern.compile("([0-9,\\.]+) (.+)").matcher(priceNode.text());
        if (m.find()) {
            return new ParsedPrice()
                .setNetAmount(m.group(1))
                .setCurrency(m.group(2));
        }
        
        return null;
    }

    /**
     * Parses list of CPVs codes from the given {@code cpvsNode}.
     *
     * @param cpvsNode
     *      node that includes CPVs data
     * @return non-empty list of parsed CPVs codes or null
     */
    public static List<ParsedCPV> parseCPVs(final Element cpvsNode) {
        if (cpvsNode == null) {
            return null;
        }

        List<ParsedCPV> cpvs = new ArrayList<>();

        Matcher m = Pattern.compile("[0-9]{8}(\\-[0-9]+)?").matcher(cpvsNode.text());
        while (m.find()) {
            cpvs.add(new ParsedCPV().setCode(m.group()));
        }
        
        return cpvs.isEmpty() ? null : cpvs;
    }

    /**
     * @param booleanNode
     *      node that includes boolean data
     * @return false only and only if text of the given {@code booleanNode} is equals to "-" (dash) or empty
     * string, null in case that {@code booleanNode} is null, otherwise true.
     */
    public static Boolean parseBoolean(final Element booleanNode) {
        if (booleanNode == null) {
            return null;
        }
        
        return !(booleanNode.text().isEmpty() || booleanNode.text().equals("-"));
    }

    /**
     * Parses publication that includes only source (URL).
     *
     * @param publicationNode
     *      node that includes publication reference data
     * @return parsed publication or null
     */
    public static ParsedPublication parsePublicationReference(final Element publicationNode) {
        if (publicationNode == null || !JsoupUtils.exists("a", publicationNode)) {
            return null;
        }

        return new ParsedPublication()
            .setIsIncluded(false)
            .setSource(PublicationSources.PT_BASE)
            .setHumanReadableUrl(JsoupUtils.selectAttribute("a", "href", publicationNode));
    }

    /**
     * The method tries to find the first value with the given label {@code regex}. In case that value exists also
     * checks whether it is equal to "-" and if yes, replace it with null.
     *
     * @see JsoupUtils#getFirstValueByLabel(org.jsoup.nodes.Element, java.lang.String)
     * 
     * @param context
     *      context in which labeled node is looked for
     * @param regex
     *      label regex
     * @return value of the labeled node
     */
    public static String getFirstValueByLabel(final Element context, final String regex) {
        String value = JsoupUtils.getFirstValueByLabel(context, regex);
        return resolveNull(value);
    }

    /**
     * Returns text of first matched element. In case that value exists also checks whether it is equal to "-"
     * and if yes, replace it with null.
     *
     * @see JsoupUtils#selectText(java.lang.String, org.jsoup.nodes.Element, boolean)
     * 
     * @param selector
     *      selector query
     * @param context
     *      context element
     * @return text content of matched element or null
     */
    public static String selectText(final String selector, final Element context) {
        String value = JsoupUtils.selectText(selector, context, false);
        return resolveNull(value);
    }

    /**
     * @param value
     *      value
     * @return original {@code value} only and only if it isn't equal to null or to character "-", otherwise null
     */
    public static String resolveNull(final String value) {
        return (value == null || value.matches("(\\-|Não aplicável|Não Houve|Não definido)[\\.]?")) ? null : value;
    }

    /**
     * Checks if passed string is in the NIF format (see https://pt.wikipedia.org/wiki/N%C3%BAmero_de_identifica%C3%A7%C3%A3o_fiscal).
     * @param s
     *      string to be checked
     * @return whether passed string is in the NIF format
     */
    public static boolean isNIF(final String s) {
        try {
            Integer.parseInt(s);
        } catch(NumberFormatException e){
            return false;
        }

        if (s.length() != 9) {
            return false;
        }
        int checkSum = 0;
        for (int i = 2; i <= 9; i++) {
            checkSum = checkSum + i * Integer.parseInt(s.substring(9 - i, 10 - i));
        }
        int remainder = checkSum % 11;
        int controlDigit = remainder <= 1 ? 0 : 11 - remainder;

        return Integer.parseInt(s.substring(s.length() - 1)) == controlDigit;
    }
}
