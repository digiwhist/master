package eu.digiwhist.worker.cz.parsed;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.commons.lang.StringUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.TextNode;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Common and utility functions for Vestnik Parser.
 */
public final class VestnikTenderParserUtils {

    private static final Logger logger = LoggerFactory.getLogger(VestnikTenderParserUtils.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VestnikTenderParserUtils() {
        throw new AssertionError();
    }

    /**
     * Returns form type (form number, eg. 2, 3, 6,...) from metadata in {@code rawTender}.
     *
     * @param rawTender
     *         raw tender with downloaded source data and metadata
     *
     * @return form number which specifies the form type (eg. 2, 3, 6,...)
     */
    protected static String getFormType(final RawData rawTender) {
        return (String) rawTender.getMetaData().get("formNumber");
    }

    /**
     * Gets form publication date from the form header.
     *
     * @param form
     *         parsed document tree for the source HTML page
     *
     * @return publication date of given parsed form
     */
    protected static String getFormPublicationDate(final Document form) {
        return parseInfoFromHeader(form, "Datum uveřejnění ve VVZ");
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
        final Element header = form.select("div.iform-header").first();
        if (header == null) {
            return null;
        }
        for (final TextNode headerNode : header.textNodes()) {
            if (headerNode.text().contains(infoTitle)) {
                return headerNode.text().split(":")[1].trim();
            }
        }
        return null;
    }

    /**
     * Parses organization number (ICO) from form header. This can be either
     * buyer id or supplier id which is specified by {@code bodyType} parameter.
     * These ids are most likely only present in contract award notices.
     *
     * @param form
     *         parsed document tree for the source HTML page
     * @param bodyType
     *         type of body as used in the form header, possible values are
     *         "zadavatele" for buyer and "dodavatele" for supplier
     *
     * @return body identifier from form header. Null if no id could be found
     * (none is present or {@code bodyType} contains invalid string).
     */
    protected static String getBodyIdFromFormHeader(final Document form, final String bodyType) {
        final Element headerOrganizationIds = form.select("div.form-ico-info").first();
        if (headerOrganizationIds != null) {
            for (final TextNode organizationIdNode : headerOrganizationIds.textNodes()) {
                if (organizationIdNode.text().contains(bodyType)) {
                    return organizationIdNode.text().split(":")[1].trim();
                }
            }
        }
        logger.warn("No organization ID for {} was found in header section.", bodyType);
        return null;
    }

    /**
     * Finds and returns element (div) containing HTML fragment with specified subsection. Subsection number should
     * be used for {@code subsectionNameRegex}.
     *
     * @param form
     *         parsed document tree for the source HTML page
     * @param subsectionNameRegex
     *         regular expression matching the subsection heading (usually containing subsection number)
     *
     * @return div element for the subsection matching given regular expression or null if no such subsection found
     */
    protected static Element getFormSubsectionByName(final Document form, final String subsectionNameRegex) {
        return form.select(String.format("div.section:has(div.iform-subsection:matches(%s))", subsectionNameRegex))
                .first();
    }

    /**
     * Parses body details from given form section with body information.
     *
     * @param bodySection
     *         HTML fragment for section containing information about body
     *
     * @return parsed information about body
     */
    protected static ParsedBody parseBody(final Element bodySection) {
        if (bodySection == null) {
            logger.warn("Unable to parse body information. Provided HTML element is null.");
            return null;
        }

        ParsedBody body = new ParsedBody();
        String ico = getFieldValue(bodySection, ".*Items\\.((IdentifikacniCislo)|(Ic(o)?))_(.*)");

        if (StringUtils.isNotEmpty(ico)) {
            body.addBodyId(new BodyIdentifier().setId(ico)
                    .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                    .setScope(BodyIdentifier.Scope.CZ));
        }

        body.setName(getFieldValue(bodySection, ".*Items\\.(((Uredni)?Nazev)|(NazevDodavatele))_(.*)"))
                .setEmail(getFieldValue(bodySection, ".*Items\\.((E_Mail)|(Emai(l)?))_(.*)"))
                .setPhone(getFieldValue(bodySection, ".*Items\\.(Telefon|Tel)_(.*)"))
                .setContactName(getFieldValue(bodySection, ".*Items\\.(KRukam|Krukam)_(.*)"))
                .setContactPoint(getFieldValue(bodySection, ".*Items\\.Kontaktni((Mist(a|o))|(Udaje))_(.*)"))
                .setAddress(
                        new ParsedAddress().setStreet(getFieldValue(bodySection, ".*Items\\.(Postovni)?Adresa_(.*)"))
                                .setCity(getFieldValue(bodySection, ".*Items\\.Obec_(.*)"))
                                .setPostcode(getFieldValue(bodySection, ".*Items\\.Psc_(.*)"))
                                .setCountry(getSelectedOptionValue(bodySection, ".*Items\\.Stat_(.*)"))
                                .setUrl(getFieldValue(bodySection, ".*Items\\.(InternetovaAdresa|ObecnaAdresa" +
                                        "(VerejnehoZadavatele|Koncesionare))_(.*)"))
                );

        return body;
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
     * Matches the {@code nameAttrRegex} regular expression against {@code name} attribute to find first {@code
     * input} element that is checked and returns associated label text.
     *
     * @param root
     *         element used as the starting context for searching
     * @param nameAttrRegex
     *         regular expression used for matching the value of {@code name} attribute of checked {@code input} element
     *
     * @return label text for matched checked {@code input} element or null if no such element found
     */
    protected static String getCheckedOptionLabel(final Element root, final String nameAttrRegex) {
        final Element checkedRadioButton = root.select(
                String.format("div.iform-field > input[name~=%s][checked]", nameAttrRegex)).first();
        return getLabelForField(checkedRadioButton, root);
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
     * Gets text value corresponding to a label.
     *
     * @param root
     *         element used as the starting context for searching
     * @param elementIdRegex
     *         regular expression matching the id of element after label or matching the {@code for} attribute of
     *         that label element
     *
     * @return text value of element preceeded by label or null if not found
     */
    protected static String getTextAfterLabel(final Element root, final String elementIdRegex) {
        final Element textField = root.select(String.format("div.iform-field > [id~=%s]", elementIdRegex)).first();
        final String textFieldValue = getElementContent(textField);
        if (textFieldValue != null) {
            return textFieldValue;
        } else {
            return getElementContent(
                    root.select(String.format("div.iform-label:has(label[for~=%s]) + div.iform-field", elementIdRegex))
                            .first());
        }
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
     * Finds all form sections by given {@code sectionNameRegex}. For each section, all the relevant elements (the
     * content of that section) are wrapped by a new {@code section} element, because in the source data all the
     * sections and relevant elements are siblings (there is no hierarchy in HTML code for individual sections).
     *
     * @param form
     *         parsed document tree for the source HTML page
     * @param sectionNameRegex
     *         regular expression matching the section heading (usually containing section number or title)
     *
     * @return list of wrapped sections found for given {@code sectionNameRegex} or empty list if nothing found
     */
    protected static Elements getFormSections(final Document form, final String sectionNameRegex) {
        Elements sections = new Elements();

        Elements allSections = form.select(String.format("div.iform-section:matches(^%s)", sectionNameRegex));
        if (!allSections.isEmpty()) {
            for (Element actualSection : allSections) {
                // list for collecting elements that belong tu actual section
                Elements actualSectionElements = new Elements();
                // first element is the one with section name
                actualSectionElements.add(actualSection);

                Element sectionElement = actualSection.nextElementSibling();
                // go through all the following siblings until there is no other sibling or until there is a new
                // section (div.iform-section)
                while (sectionElement != null && !(sectionElement.tagName()
                        .equalsIgnoreCase("div") && sectionElement.hasClass("iform-section"))) {
                    // add element to actual section
                    actualSectionElements.add(sectionElement);
                    // get next sibling
                    sectionElement = sectionElement.nextElementSibling();
                }

                // wrap section elements and add to list of sections
                sections.add(wrapElements(form, actualSectionElements, "section"));
            }
        }
        return sections;
    }

    /**
     * Creates a new element with given name and provided form's base URI and sets provided content as its inner HTML.
     *
     * @param form
     *         parsed document tree for the source HTML page - used for creating new wrapper element with the same
     *         base URI
     * @param content
     *         elements to be wrapped
     * @param wrapperElementName
     *         wrapper element name
     *
     * @return new element with {@code content} as its inner HTML
     */
    private static Element wrapElements(final Document form, final Elements content, final String wrapperElementName) {
        return form.createElement("section").html(content.outerHtml());
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
