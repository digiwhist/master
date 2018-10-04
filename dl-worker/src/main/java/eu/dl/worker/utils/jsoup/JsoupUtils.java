package eu.dl.worker.utils.jsoup;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

/**
 * Class provides functions for safe parsing values using Jsoup library.
 *
 * @author Tomas Mrazek
 */
public final class JsoupUtils {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private JsoupUtils() {
        throw new AssertionError();
    }

    /**
     * Find matching elements.
     *
     * @param selector
     *      selector query
     * @param context
     *      context element
     * @return matched elements or null
     */
    public static Elements select(final String selector, final Element context) {
        return (context == null ? null : context.select(selector.trim()));
    }

    /**
     * Find first matching element.
     *
     * @param selector
     *      selector query
     * @param context
     *      context element
     * @return matched element or null
     */
    public static Element selectFirst(final String selector, final Element context) {
        final Elements elements = select(selector, context);
        return (elements == null ? null : elements.first());
    }

    /**
     * Returns text of first matched element.
     *
     * @param selector
     *      selector query
     * @param context
     *      context element
     * @param returnOnlyString
     *      if set to true method returns an empty string instead of null if the element is not found
     * @return text content of matched element or null
     */
    public static String selectText(final String selector, final Element context,
            final boolean returnOnlyString) {
        final Element element = selectFirst(selector, context);
        if (element == null) {
            return (returnOnlyString ? "" : null);
        }

        return element.text();
    }

    /**
     * Returns text of first matched element.
     *
     * @param selector
     *      selector query
     * @param context
     *      context element
     * @return text content of matched element or null
     */
    public static String selectText(final String selector, final Element context) {
        return selectText(selector, context, false);
    }

    /**
     * Returns own text of first matched element.
     *
     * @param selector
     *      selector query
     * @param context
     *      context element
     * @return own text content of matched element or null
     */
    public static String selectOwnText(final String selector, final Element context) {
        final Element element = selectFirst(selector, context);
        if (element == null) {
            return null;
        }

        return element.ownText();
    }

    /**
     * Returns attribute value of first matched element.
     *
     * @param selector
     *      selector query
     * @param attribute
     *      attribute name
     * @param context
     *      context element
     * @return attribute value of matched element or null
     */
    public static String selectAttribute(final String selector, final String attribute, final Element context) {
        final Element element = selectFirst(selector, context);
        return ((element == null || !element.hasAttr(attribute)) ? null : element.attr(attribute));
    }

    /**
     * Returns attribute value of context element.
     *
     * @param attribute
     *      attribute name
     * @param context
     *      context element
     * @return attribute value or null
     */
    public static String selectAttribute(final String attribute, final Element context) {
        return ((context == null || !context.hasAttr(attribute)) ? null : context.attr(attribute));
    }

    /**
     * Checks whether element exists in given context.
     *
     * @param selector
     *      selector query
     * @param context
     *      context element
     * @return true if element exists, otherwise false
     */
    public static Boolean exists(final String selector, final Element context) {
        return (selectFirst(selector, context) != null);
    }

    /**
     * Checks whether element exists in given context and returns appropriate value.
     *
     * @param selector
     *      selector query
     * @param context
     *      context element
     * @param trueValue
     *      returned value if element exists
     * @param falseValue
     *      returned value if element not exists
     * @return trueValue if element exists, otherwise falseValue
     */
    public static String exists(
            final String selector,
            final Element context,
            final String trueValue,
            final String falseValue) {
        return (exists(selector, context) ? trueValue : falseValue);
    }

    /**
     * Returns combined text of all the matched elements in the given context.
     *
     * @param selector
     *      selector query
     * @param context
     *      context element
     * @return text or null if no elements matched
     */
    public static String selectCombinedText(final String selector, final Element context) {
        final Elements nodes = JsoupUtils.select(selector, context);
        return ((nodes == null || nodes.isEmpty()) ? null : nodes.text());
    }

    /**
     * Returns true only, and only if the given {@code node} has defined an {@code attribute} and its value is equals to
     * {@code expectedValue}.
     *
     * @param node
     *          node
     * @param attribute
     *          attribute name
     * @param expectedValue
     *          expected attribute value
     * @return true only, and only if the node has defined an attribute and its value is equals to expected value
     */
    public static Boolean hasAttribute(final Element node, final String attribute, final String expectedValue) {
        final String value = selectAttribute(attribute, node);
        return (value != null && expectedValue != null && value.endsWith(expectedValue));
    }

    /**
     * Returns uppercase tag name of first matched element.
     *
     * @param selector
     *      selector query
     * @param context
     *      context element
     * @return uppercase tag name or null if no element matched
     */
    public static String selectTagName(final String selector, final Element context) {
        final Element element = JsoupUtils.selectFirst(selector, context);
        return (element != null ? element.tagName().toUpperCase() : null);
    }

    /**
     * @param document
     *      document
     * @return root element of the given document
     */
    public static Element getRoot(final Element document) {
        return JsoupUtils.selectFirst(":root", document);
    }
    
    /**
     * Tries to find value nodes for the given {@code label} in the given {@code context}. In this {@code context}
     * attempts to find elements whose own text macthes label {@code regex} and if such elements exist returns list of
     * their next siblings. The label node isn't necessarily direct descendant of the {@code context} node.
     * 
     * @param context
     *      context in which labeled nodes are looked for
     * @param regex
     *      label regex.
     *      <h4>Examples:</h4>
     *      <p>'(?i)^label$' - matches nodes whose own text equals to string "label" (case insensitive)</p>
     *      <p>'label' - matches nodes whose own text includes string "label" (case sensitive)</p>
     * @return list of value nodes
     */
    public static List<Element> getLabeledValueNodes(final Element context, final String regex) {
        final Elements labeledNodes = JsoupUtils.select("*:matchesOwn("+regex+") + *", context);
        return ((labeledNodes == null || labeledNodes.isEmpty()) ? null : labeledNodes);
    }

    /**
     * Tries to find first value node for the given label {@code regex} in the given {@code context}.
     *
     * @see JsoupUtils#getLabeledValueNodes(org.jsoup.nodes.Element, java.lang.String)
     * 
     * @param context
     *      context in which labeled nodes are looked for
     * @param regex
     *      label regex
     * @return value node
     */    
    public static Element getFirstLabeledValueNode(final Element context, final String regex) {
        final List<Element> nodes = getLabeledValueNodes(context, regex);
        if (nodes == null) {
            return null;
        }

        return nodes.get(0);
    }
    
    /**
     * Tries to find all values with the given label {@code regex}.
     * 
     * @see JsoupUtils#getLabeledValueNodes(org.jsoup.nodes.Element, java.lang.String)
     * 
     * @param context
     *      context in which labeled nodes are looked for
     * @param regex
     *      label regex
     * @return list of values
     */
    public static List<String> getAllValuesByLabel(final Element context, final String regex) {
        final List<Element> nodes = getLabeledValueNodes(context, regex);
        if (nodes == null) {
            return null;
        }

        return nodes.stream()
            .map((node) -> node.text())
            .collect(Collectors.toList());
    }

    /**
     * Tries to find first value with the given label {@code regex}.
     * 
     * @see JsoupUtils#getLabeledValueNodes(org.jsoup.nodes.Element, java.lang.String)
     * 
     * @param context
     *      context in which labeled node is looked for
     * @param regex
     *      label regex
     * @return value of the labeled node
     */
    public static String getFirstValueByLabel(final Element context, final String regex) {
        final List<String> values = getAllValuesByLabel(context, regex);
        return (values == null ? null : values.get(0));
    }

    /**
     * Returns true only, and only if exists element with the given {@code selector} within the given {@code context}
     * and its text is equal to {@code expectedText}.
     *
     * @param selector
     *      selector query
     * @param context
     *      context element
     * @param expectedText
     *      expected text
     * @return true only, and only if element has text equal to expected value
     */
    public static Boolean hasText(final String selector, final Element context, final String expectedText) {
        final Element node = selectFirst(selector, context);
        return (node != null && node.text().equals(expectedText));
    }

    /**
     * Attempts to find element with name produced by {@code numberedNameSelectorProducer} with {@numberingStart} on
     * input. In case that the element exists increments a number by 1 and again attempts to find element. Continues
     * until it is able to find elements.
     *
     * @param numberedNameSelectorProducer
     *      function that accepts one integer input parameter and returns selector string
     * @param context
     *      context
     * @param numberingStart
     *      value used as start for numbering
     * @return all found elements or empty list
     */
    public static Elements selectNumberedElements(final Function<Integer, String> numberedNameSelectorProducer,
        final Element context, final int numberingStart) {
        final Elements elements = new Elements();
        int number = numberingStart;
        Element elm;
        while ((elm = JsoupUtils.selectFirst(numberedNameSelectorProducer.apply(number), context)) != null) {
            elements.add(elm);
            number++;
        }

        return elements;
    }

    /**
     * Attempts to find element with name produced by {@code numberedNameSelectorProducer} with 1 on input. In case that
     * the element exists increments a number by 1 and again attempts to find element (now with 2 on input). Continues
     * until it is able to find elements.
     *
     * @param numberedNameSelectorProducer
     *      function that accepts one integer input parameter and returns selector string
     * @param context
     *      context
     * @return all found elements or empty list
     */
    public static Elements selectNumberedElements(final Function<Integer, String> numberedNameSelectorProducer,
        final Element context) {
        return selectNumberedElements(numberedNameSelectorProducer, context, 1);
    }

    /**
     * Returns node name of first matched element.
     *
     * @param selector
     *      selector query
     * @param context
     *      context element
     * @return node name of matched element or null
     */
    public static String selectNodeName(final String selector, final Element context) {
        final Element element = selectFirst(selector, context);
        return (element == null ? null : element.nodeName());
    }

    /**
     * Returns node name of context element.
     *
     * @param context
     *      context element
     * @return node name or null
     */
    public static String selectNodeName(final Element context) {
        return (context == null ? null : context.nodeName());
    }

    /**
     * Returns n-th sibling of the given node.
     *
     * @param node
     *      node whose n-th sibling is looked for
     * @param n
     *      number of sibling
     * @return n-th sibling if exists or null
     */
    public static Element getNthSibling(final Element node, final int n) {
        if (node == null || n < 0) {
            return null;
        }

        Element sibling = node;
        for (int i = 1; i <= n; i++) {
            sibling = sibling.nextElementSibling();
            if (sibling == null) {
                break;
            }
        }

        return sibling;
    }
}
