package eu.datlab.worker.cz.parsed;

import org.apache.commons.lang.StringUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Common and utility functions for Vestnik Parser.
 */
final class VVZTenderParserUtils {

    private static final Logger logger = LoggerFactory.getLogger(VVZTenderParserUtils.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZTenderParserUtils() {
        throw new AssertionError();
    }

    /**
     * Gets specified value from form header section.
     *
     * @param form
     *         parsed document tree for the source HTML page
     * @param infoTitle
     *         title of information to be parsed from header
     *
     * @return value for requested {@code infoTitle} or null if not found
     */
    protected static String parseInfoFromHeader(final Document form, final String infoTitle) {
        final Element header = form.select("div#u71 > div").first();
        if (header == null) {
            return null;
        }
        for (final Element headerDiv : header.select("div.headerFloat")) {
            if (headerDiv.text().contains(infoTitle)) {
                return headerDiv.text().split(":")[1].trim();
            }
        }
        return null;
    }

    /**
     * Finds first element by value of its name attribute and returns text content of such element. If the element is
     * {@code input} element then value of its {@code value} attribute is returned rather than text content.
     *
     * @param root
     *         element used as the starting context for searching
     * @param nameAttrRegex
     *         regular expression used for matching the value of {@code name} attribute
     *
     * @return text content from the first element with {@code name} attribute matching the {@code nameAttrRegex}. If
     * the element is {@code input} element, value of {@code value} attribute is returned. Null is returned when no
     * element found.
     */
    protected static String getFieldValue(final Element root, final String nameAttrRegex) {
        final Element textField = root.select(String.format("[name~=%s]", nameAttrRegex)).first();
        return getElementContent(textField);
    }

    /**
     * Finds and returns fieldset element containing HTML fragment with specified subsection. Subsection number should
     * be used for {@code subsectionNameRegex}.
     *
     * @param root
     *         root element used for selecting subsection
     * @param subsectionNameRegex
     *         regular expression matching the subsection heading (usually containing subsection number)
     *
     * @return div element for the subsection matching given regular expression or null if no such subsection found
     */
    protected static Element getFormSubsectionByName(final Element root, final String subsectionNameRegex) {
        return root.select(String.format("div.iform-subsection:matches(%s) ~ fieldset", subsectionNameRegex)).first();
    }

    /**
     * Matches the {@code nameAttrRegex} regular expression against {@code name} attribute to find first {@code
     * input} element that is checked and returns its value.
     *
     * @param root
     *         element used as the starting context for searching
     * @param nameAttrRegex
     *         regular expression used for matching the value of {@code name} attribute of checked {@code input} element
     *
     * @return content of the {@code value} attribute from matched checked {@code input} element or null if no such
     * element found
     */
    protected static String getCheckedInputValue(final Element root, final String nameAttrRegex) {
        final Element checkedField = root.select(
                String.format("div.iform-field > input[name~=%s][checked]", nameAttrRegex)).first();
        if (checkedField == null) {
            return null;
        }
        return checkedField.attr("value");
    }

    /**
     * Checks whether any of the {@code input} elements whose {@code name} attribute matches given {@code nameAttrRegex}
     * regular expression is checked or not.
     *
     * @param root
     *         element used as the starting context for searching
     * @param nameAttrRegex
     *         regular expression for the {@code name} attribute of wanted {@code input} elements
     *
     * @return true if at least one of the input field is checked, false if it is not and null if none matching input
     * elements could be found
     */
    protected static Boolean isInputFieldChecked(final Element root, final String nameAttrRegex) {
        final Elements inputFields = root.select(String.format("div.iform-field > input[name~=%s]", nameAttrRegex));
        if (inputFields.isEmpty()) {
            return null;
        }
        return inputFields.hasAttr("checked");
    }

    /**
     * Gets text value of given element. This can be either the content of given element or value of {@code value}
     * attribute, if the element is {@code input} element.
     *
     * @param textField
     *         HTML element representing the text field
     *
     * @return text content of given element or value of {@code value} attribute, if the element is {@code input}
     * element. Null is returned if no such element found or if empty string is found.
     */
    protected static String getElementContent(final Element textField) {
        String elementContent = null;

        if (textField == null) {
            return null;
        }
        if (textField.nodeName().equalsIgnoreCase("input")) {
            elementContent = textField.attr("value");
        } else {
            elementContent = textField.text();
        }

        // return null for empty strings
        if (StringUtils.isBlank(elementContent)) {
            return null;
        }
        return elementContent;
    }

    /**
     * Gets the value of the first selected {@code option} element from {@code select} element specified by regular
     * expression matching its {@code name} attribute.
     *
     * @param root
     *         element used as the starting context for searching
     * @param nameAttrRegex
     *         regular expression for the {@code name} attribute of wanted {@code select} element
     *
     * @return value of selected {@code option} element
     */
    protected static String getSelectedOptionValue(final Element root, final String nameAttrRegex) {
        final Element optionElement = root.select(String.format("select[name~=%s] > option[selected]", nameAttrRegex))
                .first();
        if (optionElement == null) {
            return null;
        }
        return optionElement.attr("value");
    }

    /**
     * Gets text of label associated with given {@code element}.
     *
     * @param element
     *         element to find label for (typically div.iform-field)
     * @param ancestor
     *         ancestor element defining the scope for searching the label
     *
     * @return Label text for given element. Null if given element is null, does not have {@code id} attribute or
     * {@code label} element not found
     */
    protected static String getLabelForField(final Element element, final Element ancestor) {
        if (element == null || !element.hasAttr("id")) {
            return null;
        }
        final Element label = ancestor.select(String.format("div.iform-label > label[for=%s]", element.attr("id")))
                .first();
        return getElementContent(label);
    }

    /**
     * Concatenates two strings and handles null values.
     *
     * @param string1
     *         first string
     * @param string2
     *         second string
     *
     * @return given strings concatenated by whitespace, null if both null, first string when the second one is null
     * and vice versa
     */
    protected static String concatenateStrings(final String string1, final String string2) {
        if (string1 == null) {
            return string2;
        }
        if (string2 == null) {
            return string1;
        }
        return string1 + " " + string2;
    }
}
