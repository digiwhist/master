package eu.datlab.worker.hu.parsed;

import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

/**
 * Common and utility functions for KH Parser.
 */
final class KHTenderParserUtils {
    /**
     * Private constructor to make this class static.
     */
    private KHTenderParserUtils() {
    }

    /**
     * @param form
     *         document to be parsed
     *
     * @return div element of section II
     */
    static Element getSectionIIDiv(final Element form) {
        Elements sectionDivs = form.select("div.content.hirdetmenytartalom > div > div:has(div > " +
                "div:contains(II. SZAKASZ: A SZERZŐDÉS TÁRGYA))");
        assert sectionDivs.size() == 1;
        return sectionDivs.first();
    }

    /**
     * @param form
     *         document to be parsed
     *
     * @return div element of section III
     */
    static Element getSectionIIIDiv(final Element form) {
        Elements sectionDivs = form.select(
                "div.content.hirdetmenytartalom > div > div:has(div > div:contains(III. SZAKASZ))");
        assert sectionDivs.size() == 1;
        return sectionDivs.first();
    }

    /**
     * @param form
     *         document to be parsed
     *
     * @return div element of section IV
     */
    static Element getSectionIVDiv(final Element form) {
        Elements sectionDivs = form.select(
                "div.content.hirdetmenytartalom > div > div:has(div > div:contains(IV. SZAKASZ))");
        assert sectionDivs.size() == 1;
        return sectionDivs.first();
    }

    /**
     * @param form
     *         document to be parsed
     *
     * @return div element of section V
     */
    static Element getSectionVDiv(final Element form) {
        Elements sectionDivs = form.select(
                "div.content.hirdetmenytartalom > div > div:has(div > div:contains(V. SZAKASZ))");
        assert sectionDivs.size() == 1;
        return sectionDivs.first();
    }

    /**
     * @param form
     *         document to be parsed
     *
     * @return div element of section VI
     */
    static Element getSectionVIDiv(final Element form) {
        Elements sectionDivs = form.select(
                "div.content.hirdetmenytartalom > div > div:has(div > div:contains(VI. SZAKASZ))");
        assert sectionDivs.size() == 1;
        return sectionDivs.first();
    }

    /**
     * @param form
     *         document to be parsed
     *
     * @return div element of annex A
     */
    static Element getAnnexADiv(final Element form) {
        Elements annexDivs = form.select("div.content.hirdetmenytartalom > div >" +
                " div:has(div > div:contains(A. melléklet További címek és kapcsolattartási pontok))");
        assert annexDivs.size() == 1;
        return annexDivs.first();
    }

    /**
     * Gets text from element, based on Jsoup path in safe way. Number of founded elements has to be no more than 1.
     *
     * @param element
     *         Element to be searched in
     * @param selector
     *         Jsoup path to search with
     *
     * @return String from element
     */
    static String getTextFromElement(final Element element, final String selector) {
        if (element == null || selector == null) {
            assert false;
            return null;
        }

        final Elements elements = element.select(selector);
        assert elements.size() <= 1;
        return elements.isEmpty() ? null : elements.first().text().trim();
    }

    /**
     * Gets text from element, based on Jsoup selectors.
     * More selectors can get text, but the text has to be the desired text.
     * Example: Two valid selectors (set of elements from the first selector is subset of the second selector), which
     *          can be passed as second parameter:
     *            1) "div:containsOwn(A rész száma) > span"
     *            2) "div:containsOwn(Rész száma) > span:nth-child(1)"
     *
     * @param element
     *         Element to be searched in
     * @param selectors
     *         Jsoup paths to search with
     *
     * @return String from element
     */
    static String getTextFromElement(final Element element, final String[] selectors) {
        if (element == null || selectors == null) {
            assert false;
            return null;
        }

        String result = null;

        for (String selector : selectors) {
            result = getTextFromElement(element, selector);
            if (result != null) {
                break;
            }
        }

        return result;
    }

    /**
     * Gets own text from element, based on Jsoup path in safe way. Number of founded elements has to be no more than 1.
     *
     * @param element
     *         Element to be searched in
     * @param selector
     *         Jsoup path to search with
     *
     * @return String from element
     */
    static String getOwnTextFromElement(final Element element, final String selector) {
        if (element == null || selector == null) {
            assert false;
            return null;
        }

        final Elements elements = element.select(selector);
        assert elements.size() <= 1;
        return elements.isEmpty() ? null : elements.first().ownText().trim();
    }

    /**
     * Gets own text from element, based on Jsoup paths in safe way.
     * More selectors can get own text, but the text has to be the desired text.
     * Example: Two valid selectors (set of elements from the first selector is subset of the second selector), which
     *          can be passed as second parameter:
     *            1) "div:containsOwn(A rész száma) > span"
     *            2) "div:containsOwn(Rész száma) > span:nth-child(1)"
     *
     * @param element
     *         Element to be searched in
     * @param selectors
     *         Jsoup paths to search with
     *
     * @return String from element
     */
    static String getOwnTextFromElement(final Element element, final String[] selectors) {
        if (element == null || selectors == null) {
            assert false;
            return null;
        }

        String result = null;

        for (String selector : selectors) {
            result = getOwnTextFromElement(element, selector);
            if (result != null) {
                break;
            }
        }

        return result;
    }
}
