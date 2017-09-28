package eu.digiwhist.worker.hu.parsed;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

import org.jsoup.nodes.Document;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * This class is parsing contracting authority data for Hungary.
 *
 * @author Tomas Mrazek
 */
public final class KHContractBuyerHandler {
    private static final String CHECKBOX_VALUE = "checked_value";
    private static final Logger logger = LoggerFactory.getLogger(KHContractBuyerHandler.class.getName());

    /**
     * Default constructor.
     */
    private KHContractBuyerHandler() {
    }

    /**
     * Parse buyer from KHContract.
     *
     * @param parsedTender parsed tender
     * @param doc          document to parse from
     *
     * @return List<ParsedContractingAuthority>
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document doc) {

        // Select text that contains data.
        final String dataInPlainText = selectText(new String[]{
                "div.content.hirdetmenytartalom > div",
                "div.content.hirdetmenytartalom > pre"}, doc);

        if (dataInPlainText == null) {
            logger.error("No text to parse.");
            throw new UnrecoverableException("No text to parse.");
        }

        // Parse text into list of ParsedTextObjects (Section, Attribute, Value)
        final List<ParsedTextObject> allStructuredData = parseTextToParsedTextObjects(dataInPlainText);

        // Filter section that contains contracting authority data
        final List<ParsedTextObject> contractingAuthorityStructuredData = getSection(allStructuredData, new String[]{
                "cím és kapcsolattartási pont",
                "Név és címek",
                "CÍM ÉS KAPCSOLATTARTÁSI PONT",
                "ajánlatkérőként szerződő fél neve és"});

        // Pour variables to contracting authority
        parsedTender.addBuyer(new ParsedBody()
                // Text parsing here
                .setName(selectFirstValue("Hivatalos név", contractingAuthorityStructuredData))
                .setContactName(selectFirstValue("Címzett", contractingAuthorityStructuredData))
                .setContactPoint(selectFirstValue("Kapcsolattartási pon", contractingAuthorityStructuredData))
                .setAddress(new ParsedAddress()
                        .setStreet(selectFirstValue("Postai cím", contractingAuthorityStructuredData))
                        .setPostcode(selectFirstValue("Postai irányítószám", contractingAuthorityStructuredData))
                        .setCity(selectFirstValue("Város/Község", contractingAuthorityStructuredData))
                        .setCountry(selectFirstValue("Ország", contractingAuthorityStructuredData))
                        .setUrl(selectFirstValue("URL", contractingAuthorityStructuredData))
                )
                .setPhone(selectFirstValue("Telefon", contractingAuthorityStructuredData))
                .setEmail(selectFirstValue("E-mail", contractingAuthorityStructuredData))
                // Jsoup used from below
                .setBuyerType(JsoupUtils.selectText("th:containsOwn(Ajánlatkérő típusa:) + td", doc))
                .addMainActivity(JsoupUtils.selectText("th:containsOwn(Ajánlatkérő fő tevényeségi köre:) + td", doc)));

        return parsedTender;
    }

    /**
     * Parse text into list of ParsedTextObjects (Section, Attribute, Value)
     * Method works accordingly, if line (only one can happen for each line):
     * <p>
     * 1. Starts with roman numeral, regard it as section name
     * 2. Contains colon, regard text before colon as attribute, text behind colon as value
     * 3. None of above but attribute was found in section, regard text as value of last attribute (append it)
     *
     * @param data plain text to parse
     *
     * @return List<ParsedContractingAuthority>
     */
    private static List<ParsedTextObject> parseTextToParsedTextObjects(final String data) {
        final List<ParsedTextObject> resultList = new ArrayList<>();

        String currentSection = "";
        String currentAttribute = "";
        String currentValue = "";

        Scanner scanner = new Scanner(data);
        while (scanner.hasNextLine()) {

            final String currentLine = scanner.nextLine().trim();

            // Line starts with roman numeral, regard it as section name.
            // Add possible parsed attribute and value holders in resultList and erase them
            if (currentLine.matches("^(I|V)(\\.|I|V).*")) {
                if (!currentAttribute.isEmpty()) {
                    resultList.add(new ParsedTextObject(currentSection, currentAttribute, currentValue));
                    currentAttribute = "";
                    currentValue = "";
                }

                currentSection = currentLine;

                // Line contains colon, regard text before colon as attribute, text behind colon as value
                // Add possible parsed attribute and value holders in resultList and erase them
            } else if (currentLine.contains(":")) {
                if (!currentAttribute.isEmpty()) {
                    resultList.add(new ParsedTextObject(currentSection, currentAttribute, currentValue));
                    currentAttribute = "";
                    currentValue = "";
                }

                final String[] nameAndValue = currentLine.split(":");
                if (nameAndValue.length > 0) {
                    currentAttribute = nameAndValue[0];
                }
                if (nameAndValue.length > 1) {
                    currentValue = nameAndValue[1];
                }

                // Line contains only value text, append it to text in currentValue text
            } else if (!currentAttribute.isEmpty()) {
                currentValue += currentLine;
            }
        }

        return resultList;
    }

    /**
     * Select text from Document with preserved spacing.
     *
     * @param selector selector to element containing text
     * @param document document to select from
     *
     * @return String or null
     */
    private static String selectText(final String selector, final Document document) {
        final Elements elements = JsoupUtils.select(selector, document);

        if (elements == null || elements.first() == null) {
            return null;
        }

        // If text is acquired directly spacing is lost, thus getting html
        final String result = elements.first().html();

        // Remove HTML tags and empty lines
        return result == null ? null : result.replaceAll("<[^>]*>", "").replaceAll("(?m)^[ \t]*\r?\n", "");
    }

    /**
     * Select text from Document with preserved spacing.
     *
     * @param selectors selectors to element containing text
     * @param document  document to select from
     *
     * @return String or null
     */
    private static String selectText(final String[] selectors, final Document document) {
        if (selectors == null || document == null) {
            return null;
        }

        for (String selector : selectors) {
            final String result = selectText(selector, document);

            if (result != null) {
                return result;
            }
        }

        return null;
    }

    /**
     * Select value from list of ParsedTextObjects with selected section name and attribute CHECKBOX_VALUE.
     *
     * @param sectionName       section selector
     * @param parsedTextObjects objects to select from
     *
     * @return String or null
     */
    private static String getCheckBoxValue(final String sectionName, final List<ParsedTextObject> parsedTextObjects) {
        final List<ParsedTextObject> sectionObjects = getSection(parsedTextObjects, sectionName);

        if (sectionObjects == null) {
            return null;
        }

        return selectFirstValue(CHECKBOX_VALUE, sectionObjects);
    }

    /**
     * Select objects in given section.
     *
     * @param parsedTextObjects objects to select from
     * @param section           selector
     *
     * @return List<ParsedTextObjects> or null
     */
    private static List<ParsedTextObject> getSection(final List<ParsedTextObject> parsedTextObjects,
                                                     final String section) {
        if (section == null || parsedTextObjects == null) {
            return null;
        }

        return parsedTextObjects.stream().filter(t -> t.getSection().contains(section)).collect(Collectors.toList());
    }

    /**
     * Select objects in given section.
     *
     * @param parsedTextObjects objects to select from
     * @param sections          selectors
     *
     * @return List<ParsedTextObjects> or null
     */
    private static List<ParsedTextObject> getSection(final List<ParsedTextObject> parsedTextObjects,
                                                     final String[] sections) {
        if (sections == null || parsedTextObjects == null) {
            return null;
        }

        for (String section : sections) {
            final List<ParsedTextObject> result = getSection(parsedTextObjects, section);

            if (result != null && !result.isEmpty()) {
                return result;
            }
        }

        return null;
    }

    /**
     * Select first value in objects with given attribute.
     *
     * @param parsedTextObjects objects to select from
     * @param attribute         selector
     *
     * @return List<ParsedTextObjects> or null
     */
    private static String selectFirstValue(final String attribute, final List<ParsedTextObject> parsedTextObjects) {
        if (attribute == null || parsedTextObjects == null) {
            return null;
        }

        List<ParsedTextObject> list = parsedTextObjects.stream().filter(t -> t.getAttribute().contains(attribute))
                .collect(Collectors.toList());

        if (list.isEmpty()) {
            return null;
        }

        final String result = list.get(0).getValue();

        return result.replace("-", "").trim().isEmpty() ? null : result.trim();
    }

    /**
     * Select first value in objects with given attribute.
     *
     * @param parsedTextObjects objects to select from
     * @param attributes        selectors
     *
     * @return List<ParsedTextObjects> or null
     */
    private static String selectFirstValue(final String[] attributes, final List<ParsedTextObject> parsedTextObjects) {
        if (attributes == null || parsedTextObjects == null) {
            return null;
        }

        for (String attribute : attributes) {
            final String result = selectFirstValue(attribute, parsedTextObjects);

            if (result != null) {
                return result;
            }
        }

        return null;
    }

    /**
     * Parsed text object DTO, holds attribute, its variable and section its all in.
     */
    private static class ParsedTextObject {
        private String section;
        private String attribute;
        private String value;

        /**
         * Default constructor.
         *
         * @param section   name of section where attribute and value are present
         * @param attribute name of attribute
         * @param value     value
         */
        ParsedTextObject(final String section, final String attribute, final String value) {
            this.section = section;
            this.attribute = attribute;
            this.value = value;
        }

        /**
         * Getter for section.
         *
         * @return String or null
         */
        String getSection() {
            return section;
        }

        /**
         * Getter for attribute.
         *
         * @return String or null
         */
        String getAttribute() {
            return attribute;
        }

        /**
         * Getter for value.
         *
         * @return String or null
         */
        String getValue() {
            return value;
        }
    }
}
