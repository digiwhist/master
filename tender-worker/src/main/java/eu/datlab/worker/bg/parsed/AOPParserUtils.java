package eu.datlab.worker.bg.parsed;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * Class provides useful function for AOP praser.
 * 
 * @author Tomas Mrazek
 */
public final class AOPParserUtils {
    /**
     * Supress default constructor for noninstatiability.
     */
    private AOPParserUtils() {
    }
    
    /**
     * @param doc
     *      parsed document
     * @return section AI element or null
     */
    public static Element getSectionAI(final Document doc) {
        return JsoupUtils.selectFirst(".section_AI", doc);
    }

    /**
     * @param doc
     *      parsed document
     * @return section I element or null
     */
    public static Element getSectionI(final Document doc) {
        return JsoupUtils.selectFirst(".section_I", doc);
    }

    /**
     * @param doc
     *      parsed document
     * @return section II element or null
     */
    public static Element getSectionII(final Document doc) {
        return JsoupUtils.selectFirst(".section_II", doc);
    }

    /**
     * @param doc
     *      parsed document
     * @return section III element or null
     */
    public static Element getSectionIII(final Document doc) {
        return JsoupUtils.selectFirst(".section_III", doc);
    }

    /**
     * @param doc
     *      parsed document
     * @return section IV element or null
     */
    public static Element getSectionIV(final Document doc) {
        return JsoupUtils.selectFirst(".section_IV", doc);
    }

    /**
     * @param doc
     *      parsed document
     * @return section V element or null
     */
    public static Element getSectionV(final Document doc) {
        return JsoupUtils.selectFirst(".section_V", doc);
    }

    /**
     * @param doc
     *      parsed document
     * @return section VI element or null
     */
    public static Element getSectionVI(final Document doc) {
        return JsoupUtils.selectFirst(".section_VI", doc);
    }

    /**
     * Returns value by label for parameter.
     *
     * @param labelFor
     *      value of label's for parameter
     * @param context
     *      context
     * @return value or null
     */
    public static String getValueByLabel(final String labelFor, final Element context) {
        return JsoupUtils.selectText("label[for=" + labelFor + "] ~ span.input_value", context);
    }

    /**
     * Returns anything under label for parameter.
     *
     * @param labelFor
     *      value of label's for parameter
     * @param context
     *      context
     * @return value or null
     */
    public static String getAnythingUnderLabel(final String labelFor, final Element context) {
        return JsoupUtils.selectText("label[for=" + labelFor + "] + *", context);
    }

    /**
     * Returns any whole element when given partial text.
     *
     * @param selector
     *      value with selector
     * @param context
     *      context
     * @return value or null
     */
    public static String getSelf(final String selector, final Element context) {
        return JsoupUtils.selectText("*:containsOwn(" + selector + ")", context);
    }

    /**
     * Returns anything under label for parameter.
     *
     * @param selector
     *      value of P
     * @param context
     *      context
     * @return value or null
     */
    public static String getTableUnderTitle(final String selector, final Element context) {
        return JsoupUtils.selectText("p:contains(" + selector + ") + table", context);
    }

    /**
     * Checks whether a property with the given label is enabled. The enabled property has value set to ДА,
     * disabled to НЕ.
     *
     * @param labelFor
     *      value of label's for parameter
     * @param context
     *      context
     * @return true/false for existing labeled value, or null
     */
    public static Boolean isEnabled(final String labelFor, final Element context) {
        String value = getValueByLabel(labelFor, context);
        return value == null ? null : value.equals("ДА");
    }

    /**
     * Parses price. Method assumes that in the given context exists a label with attribut 'for' equal to
     * {@code <prefix>cost} and optionally also {@code <prefix>currency}.
     * 
     * @see AOPParserUtils#getValueByLabel(java.lang.String, org.jsoup.nodes.Element)
     *
     * @param prefix
     *      prefix of the label
     * @param context
     *      context taht includes price data
     * @return parsed price or null
     */
    public static ParsedPrice parseInlinePrice(final String prefix, final Element context) {
        String amount = getValueByLabel(prefix + "cost", context);

        return amount == null ? null : new ParsedPrice()
            .setNetAmount(amount)
            .setCurrency(getValueByLabel(prefix + "currency", context));
    }
}
