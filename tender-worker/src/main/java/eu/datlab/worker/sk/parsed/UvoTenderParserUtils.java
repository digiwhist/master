package eu.datlab.worker.sk.parsed;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utils for UVO tender parsed.
 *
 * @author Michal Riha
 */
public final class UvoTenderParserUtils {

    static final Pattern NPWP_REASONS_REGEX = Pattern.compile("[a-i]\\d?\\)");

    /**
     * Logger.
     */
    private static final Logger logger = LoggerFactory.getLogger(UvoTenderParserUtils.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private UvoTenderParserUtils() {
        throw new AssertionError();
    }

    /**
     * Gets type of form for given raw tender publication.
     *
     * @param formCode
     *         code of form
     *
     * @return type of form for provided raw tender publication
     */
    static PublicationFormType getFormType(final String formCode) {
        if (formCode.isEmpty()) {
            throw new UnrecoverableException("Could not determine the type of form.");
        } else {
            if (formCode.startsWith("M") || formCode.contains("POT") || formCode.startsWith("WY")) {
                return PublicationFormType.CONTRACT_NOTICE;
            } else if (formCode.startsWith("V") || formCode.startsWith("IP")) {
                return PublicationFormType.CONTRACT_AWARD;
            } else if (formCode.startsWith("Z")) {
                return PublicationFormType.CONTRACT_CANCELLATION;
            } else if (formCode.matches("DO(P|S|T)")) {
                return PublicationFormType.CONTRACT_AMENDMENT;
            } else {
                return PublicationFormType.OTHER;
            }
        }
    }

    /**
     * Gets string from element, based on Jsoup path in safe way.
     *
     * @param element
     *         Element to be searched in
     * @param selector
     *         Jsoup path to search with
     *
     * @return String from element
     */
    static String getFirstValueWithoutDots(final Element element, final String selector) {
        String result = getFirstValueFromElement(element, selector);

        if (result == null) {
            return null;
        }

        return result.replaceAll("\\.", "");
    }

    /**
     * Gets string from element, based on Jsoup path in safe way.
     *
     * @param element
     *         Element to be searched in
     * @param selector
     *         Jsoup path to search with
     *
     * @return String from element
     */
    static String getFirstValueWithoutDots(final Element element, final String[] selector) {
        String result = getFirstValueFromElement(element, selector);

        if (result == null) {
            return null;
        }

        return result.replaceAll("\\.", "");
    }

    /**
     * Gets string from element, based on Jsoup path in safe way.
     *
     * @param element
     *         Element to be searched in
     * @param selector
     *         Jsoup path to search with
     *
     * @return String from element
     */
    static String getFirstValueFromElement(final Element element, final String selector) {
        if (element == null) {
            return null;
        }

        final Elements elements = element.select(selector);

        if (elements != null && !elements.isEmpty()) {
            for (Element result : elements) {
                if (!result.text().trim().isEmpty()) {
                    return result.text().trim();
                }
            }
        }

        return null;
    }

    /**
     * Gets string from element, based on Jsoup paths in safe way.
     *
     * @param element
     *         Element to be searched in
     * @param selectors
     *         Jsoup paths to search with
     *
     * @return String from element
     */
    static String getFirstValueFromElement(final Element element, final String[] selectors) {
        if (selectors == null) {
            return null;
        }

        String result = null;

        for (String selector : selectors) {
            result = getFirstValueFromElement(element, selector);

            if (result != null && !result.isEmpty()) {
                break;
            }
        }

        return result;
    }

    /**
     * Gets string from element, based on Jsoup path in safe way.
     *
     * @param element
     *         Element to be searched in
     * @param selector
     *         Jsoup path to search with
     *
     * @return String from element
     */
    static String getFirstOwnValueFromElement(final Element element, final String selector) {
        if (element == null) {
            return null;
        }

        final Elements elements = element.select(selector);

        if (elements != null && !elements.isEmpty()) {
            for (Element result : elements) {
                if (!result.ownText().trim().isEmpty()) {
                    return result.ownText().trim();
                }
            }
        }

        return null;
    }

    /**
     * Gets first element from given element, based on Jsoup path in safe way.
     *
     * @param element
     *         Element to be searched in
     * @param selector
     *         Jsoup path to search with
     *
     * @return Element or null
     */
    static Element getFirstElement(final Element element, final String selector) {
        if (element == null || selector == null || selector.isEmpty()) {
            return null;
        }

        final Elements elements = element.select(selector);

        return elements.isEmpty() ? null : elements.first();
    }

    /**
     * Gets string from element, based on Jsoup paths in safe way.
     *
     * @param element
     *         Element to be searched in
     * @param selectors
     *         Jsoup paths to search with
     *
     * @return String from element
     */
    static String getFirstOwnValueFromElement(final Element element, final String[] selectors) {
        if (selectors == null) {
            return null;
        }

        String result = null;

        for (String selector : selectors) {
            result = getFirstOwnValueFromElement(element, selector);

            if (result != null && !result.isEmpty()) {
                break;
            }
        }

        return result;
    }

    /**
     * Gets string of true or false value, based on local yes word.
     *
     * @param element
     *         Element to be searched in
     * @param selector
     *         Jsoup paths to search with
     *
     * @return String true, String false, null
     */
    static String getTrueOrFalseFromElement(final Element element, final String selector) {
        String booleanInString = getFirstValueFromElement(element, selector);

        return booleanInString == null
                ? null : String.valueOf(booleanInString.toLowerCase().contains("Áno".toLowerCase()));
    }

    /**
     * Gets string of true or false value, based on local yes word.
     *
     * @param element
     *         Element to be searched in
     * @param selectors
     *         Jsoup paths to search with
     *
     * @return String true, String false, null
     */
    static String getTrueOrFalseFromElement(final Element element, final String[] selectors) {
        String booleanInString = getFirstValueFromElement(element, selectors);

        return booleanInString == null
                ? null : String.valueOf(booleanInString.toLowerCase().contains("Áno".toLowerCase()));
    }

    /**
     * Gets strings from element, based on Jsoup path in safe way.
     *
     * @param element
     *         Element to be searched in
     * @param selector
     *         Jsoup path to search with
     *
     * @return String[] or null
     */
    static List<String> getValuesFromElement(final Element element, final String selector) {
        if (element == null) {
            //            logger.error("Received element is null.");
            return null;
        }

        final Elements elements = element.select(selector);
        if (elements.isEmpty()) {
            return null;
        }

        final List<String> stringsFromElements = new ArrayList<>();

        for (Element tempElement : elements) {
            if (!tempElement.text().trim().isEmpty()) {
                stringsFromElements.add(tempElement.text().trim());
            }
        }

        return stringsFromElements.isEmpty() ? null : stringsFromElements;
    }

    /**
     * Gets strings from element, based on Jsoup path in safe way.
     * Multiple selector are inserted, first funcational is used.
     *
     * @param element
     *         Element to be searched in
     * @param selectors
     *         Jsoup path to search with
     *
     * @return String from element
     */
    static List<String> getValuesFromElement(final Element element, final String[] selectors) {
        if (selectors == null) {
            return null;
        }

        List<String> result = null;

        // do not continue as soon as selector is able to retrieve data
        for (String selector : selectors) {
            result = getValuesFromElement(element, selector);

            if (result != null && !result.isEmpty()) {
                break;
            }
        }

        return result;
    }

    /**
     * Default method to parse price and select value without or with VAT.
     *
     * @param element
     *         element to parse
     * @param vatSelector
     *         selector select between vat and netAmount
     * @param priceSelector
     *         price selector
     * @param currencySelector
     *         currency selector
     * @param vatAmountSelector
     *         vatAmount selector
     *
     * @return parsed price
     */
    public static ParsedPrice parsePrice(final Element element, final String vatSelector, final String
            priceSelector, final String currencySelector, final String vatAmountSelector) {
        return parsePrice(element, new String[]{vatSelector}, new String[]{priceSelector},
                new String[]{currencySelector}, new String[]{vatAmountSelector});
    }

    /**
     * Default method to parse price and select value without or with VAT.
     *
     * @param element
     *         element to parse
     * @param vatSelector
     *         selector select between vat and netAmount
     * @param priceSelector
     *         price selector
     * @param currencySelector
     *         currency selector
     * @param vatAmountSelector
     *         vatAmount selector
     *
     * @return parsed price
     */
    public static ParsedPrice parsePrice(final Element element, final boolean vatSelector, final String
            priceSelector, final String currencySelector, final String vatAmountSelector) {
        return parsePrice(element, vatSelector, new String[]{priceSelector},
                new String[]{currencySelector}, new String[]{vatAmountSelector});
    }

    /**
     * Default method to parse price and parse value with or without VAT.
     *
     * @param element
     *         element to parse
     * @param vatSelector
     *         selector select between vat and netAmount
     * @param priceSelector
     *         price selector
     * @param currencySelector
     *         currency selector
     * @param vatAmountSelector
     *         vatAmount selector
     *
     * @return parsed price
     */
    public static ParsedPrice parsePrice(final Element element, final String[] vatSelector, final String[]
            priceSelector, final String[] currencySelector, final String[] vatAmountSelector) {
        String withOrWithoutVAT = getFirstValueFromElement(element, vatSelector);

        if (withOrWithoutVAT == null || withOrWithoutVAT.toLowerCase().contains("bez")) {
            return parsePrice(element, false, priceSelector, currencySelector, vatAmountSelector);
        } else if (withOrWithoutVAT.toLowerCase().contains("s dph") || withOrWithoutVAT.toLowerCase()
                .contains("vrátane")) {
            return parsePrice(element, true, priceSelector, currencySelector, vatAmountSelector);
        } else {
            return null;
        }
    }

    /**
     * Default method to parse price value with or without VAT.
     *
     * @param element
     *         element to parse
     * @param withVAT
     *         selector select between vat and netAmount
     * @param priceSelector
     *         price selector
     * @param currencySelector
     *         currency selector
     * @param vatAmountSelector
     *         vatAmount selector
     *
     * @return parsed price
     */
    public static ParsedPrice parsePrice(final Element element, final boolean withVAT, final String[]
            priceSelector, final String[] currencySelector, final String[] vatAmountSelector) {
        if (element == null) {
            return null;
        }

        // If there is no price other values are useless, return null
        final String price = getFirstValueFromElement(element, priceSelector);
        if (price == null) {
            return null;
        }

        // currency should never be number
        String currency = getFirstValueFromElement(element, currencySelector);
        if (currency != null && currency.matches(".*\\d+.*")) {
            currency = null;
        }

        // Parse VAT value safely (check valid selector)
        String vatAmount = null;
        if (vatAmountSelector != null && vatAmountSelector.length != 0 && vatAmountSelector[0] != null) {
            vatAmount = getFirstValueFromElement(element, vatAmountSelector);
        }

        if (withVAT) {

            return new ParsedPrice()
                    .setAmountWithVat(price)
                    .setCurrency(currency)
                    .setVat(vatAmount);
        } else {
            return new ParsedPrice()
                    .setNetAmount(price)
                    .setCurrency(currency)
                    .setVat(vatAmount);

        }
    }

    /**
     * Get value for variable which is X positions behind name of value in corrections.
     *
     * @param strings strings to get value from
     * @param position position of value name
     * @param positionsBehind position behind the value name
     * @return String or null
     */
    public static String getCorrectionValue(final List<String> strings, final int position, final int positionsBehind) {
        if (position + positionsBehind < strings.size()) {
            return strings.get(position + positionsBehind);
        } else {
            return null;
        }
    }

    /**
     * Parses npwp reasons from input string.
     *
     * @param input
     *      input string to be searched
     * @return list of reasons
     */
    public static List<String> getNpwpReasons(final String input) {
        List<String> reasons = new ArrayList<>();

        Matcher m = NPWP_REASONS_REGEX.matcher(input);
        while (m.find()) {
            reasons.add(m.group());
        }

        return reasons;
    }

    /**
     * Removes lot subtitles from subsections II.2.2, V.2.3, II.2.5 and II.2.1. In these sections subtitle 'Časť:'
     * doesn't mean lot.
     *
     * @param document
     *      document
     */
    public static void removeFakeLots(final Element document) {
        List<String> selectors = Arrays.asList("div.subtitle:contains(II.2.2)", "div.subtitle:contains(V.2.3)",
            "div.subtitle:contains(II.2.5)", "div.subtitle:contains(II.2.1)");
        Element oddielII = document.selectFirst("fieldset:contains(ODDIEL II:)");
        Element oddielV = document.selectFirst("fieldset:contains(ODDIEL V:)");

        for (String selector : selectors) {
            Element oddiel = selector.contains("V") ? oddielV : oddielII;
            if(oddiel == null) {
                continue;
            }
            Elements sections = oddiel.select(selector);
            Elements endElements = oddiel.select(selector + " ~ div.subtitle:matches([IVX]+\\.\\d)");
            for(Element section: sections) {
                if (section == null) {
                    continue;
                }

                int sectionStart = section.siblingIndex();

                // looking for first end element with larger index than sectionStart and remove seen elements
                Element endElement = null;
                while(endElement == null) {
                    if(endElements.isEmpty()) {
                        break;
                    }
                    if(endElements.get(0).siblingIndex() > sectionStart) {
                        endElement = endElements.get(0);
                    }
                    endElements.remove(0);
                }
                int sectionEnd = endElement != null ? endElement.siblingIndex() : section.lastElementSibling().siblingIndex();

                Elements lots = oddiel.select("span:containsOwn(Časť:), span:containsOwn(Časť č.:)");
                for (Element l : lots) {
                    if (sectionEnd != -1 && l.siblingIndex() >= sectionEnd) {
                        break;
                    }

                    if (l.siblingIndex() > sectionStart && l.siblingIndex() < sectionEnd) {
                        logger.info("Removing {} from section {} of publication {}, fake lot.", l.html(),
                                selector.split("\\(")[1].split("\\)")[0]);
                        l.text("");
                    }
                }
            }
        }
    }

    /**
     * Parse lot number or lot.
     *
     * @param lot lot to parse from
     * @return String or null
     */
    public static String parseLotNumber(final Element lot) {
        String lotNumber = getFirstValueFromElement(lot, "span:matchesOwn(Časť: ?\\d+)");
        if (lotNumber == null) {
            lotNumber = getFirstValueFromElement(lot, "div:matchesOwn(Časť č.: ?\\d+)");
        }

        return lotNumber == null ? null : lotNumber.replace("Časť:", "").replace("Časť č.:", "").trim();
    }

    /**
     * Parse raw address of bidder from document.
     *
     * @param lot lot to be parsed
     * @return parsed raw address
     */
    public static ParsedAddress parseRawAddress(final Element lot) {
        Element element = lot.selectFirst("div.ContactSelectList > span.titleValue > text");

        if (element == null) {
            element = lot.selectFirst("div.ContactSelectList > span.titleValue");
        }

        if (element == null) {
            return null;
        }

        Element nuts = lot.selectFirst("span:containsOwn(Kód NUTS:) + span");
        Element country = lot.selectFirst("span:containsOwn(Slovensko)");

        return new ParsedAddress()
                .addNuts(nuts == null ? null : nuts.ownText())
                .setCountry(country == null ? null : country.ownText())
                .setRawAddress(element.ownText());
    }

    /**
     * Parses lot bidders.
     *
     * @param lot to be parsed
     * @return list of ParsedBody or null
     */
    public static List<ParsedBody> parseBidders(final Element lot) {
        Elements nodes = JsoupUtils.select("div.contactSelectList", lot);

        List<ParsedBody> bidders = new ArrayList<>();
        for (Element n : nodes) {
            ParsedBody parsedBody = new ParsedBody()
                    .setName(getFirstValueFromElement(n, new String[]{
                            "div.contactSelectList > span.titleValue > span",
                            "div.contactSelectList > span.titleValue"}))
                    .setAddress(parseRawAddress(n))
                    .setIsSme(getTrueOrFalseFromElement(n.nextElementSibling(), "div:containsOwn(Dodávateľom je MS) > span"))
                    .setPhone(getFirstValueFromElement(n, "span:containsOwn(Telefón:) + span"))
                    .setEmail(getFirstValueFromElement(n, "span:containsOwn(Email) + span"));

            String bodyId = getFirstValueFromElement(n, new String[]{
                    "span:containsOwn(IČO:) + span",
                    "span:containsOwn(útroštátne identifikačné číslo:) + span"});

            if (bodyId != null) {
                parsedBody.setBodyIds(Arrays.asList(new BodyIdentifier()
                        .setId(bodyId)
                        .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                        .setScope(BodyIdentifier.Scope.SK)));
            }

            bidders.add(parsedBody);
        }

        return bidders.isEmpty() ? null : bidders;
    }


}
