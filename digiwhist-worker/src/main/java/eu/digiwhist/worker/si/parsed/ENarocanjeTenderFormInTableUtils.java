package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.StringUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Utilities for ENarocanje forms which information are saved in table HTML element.
 * All the "table" handlers can use the utility methods from this one place. It is better than to have the methods in
 * base class, because some methods are handler specific.
 *
 * @author Marek Mikes
 */
final class ENarocanjeTenderFormInTableUtils {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeTenderFormInTableUtils() {
    }

    // pattern contains only "contains" keyword to find all sections - lot title is in the "td" element
    static final String SECTION_SELECTOR_PATTERN = "div.tab-content > table > tbody > tr:matches(%s)";
    static final String SECTION_FIRST_ELEMENT_SELECTOR_PATTERN = SECTION_SELECTOR_PATTERN + " > td > *:first-child";

    /**
     * Gets elements representing section (title and content).
     *
     * @param sectionNumber
     *          number of section in title
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return elements representing section
     */
    static Elements getSections(final String sectionNumber, final Element form) {
        return JsoupUtils.select(String.format(SECTION_SELECTOR_PATTERN, sectionNumber), form);
    }

    /**
     * Gets element representing section (title and content).
     *
     * @param sectionNumber
     *          number of section in title
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return element representing section
     */
    static Element getSection(final String sectionNumber, final Element form) {
        return JsoupUtils.selectFirst(String.format(SECTION_SELECTOR_PATTERN, sectionNumber), form);
    }

    /**
     * Gets wrapped element representing section (title and content) where each node is separated by <br>.
     *
     * @param sectionNumber
     *          number of section in title
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return wrapped element representing section where each node is separated by <br>
     */
    static Element getSectionWithSeparatedNodes(final String sectionNumber, final Element form) {
        return ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(String.format(SECTION_FIRST_ELEMENT_SELECTOR_PATTERN, sectionNumber), form),
                null);
    }

    /**
     * Gets section content without title. The method suppose that title ends by ":".
     *
     * @param sectionNumber
     *          number of section in title
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return section content without title
     */
    static String getSectionContent(final String sectionNumber, final Element form) {
        // we suppose that colon is at the end of section title
        return getSectionContent(sectionNumber, form, Collections.singletonList(":"));
    }

    /**
     * Gets section content without title.
     *
     * @param sectionNumber
     *          number of section in title
     * @param form
     *         parsed document for the source HTML page (parsed form)
     * @param contentSeparators
     *         separators of title and section content to get just the content. It can be the whole title.
     *
     * @return section content without title
     */
    static String getSectionContent(final String sectionNumber, final Element form,
                                    final List<String> contentSeparators) {
        final Element section = getSection(sectionNumber, form);
        if (section == null) {
            return null;
        }
        final String sectionText = section.text();

        final String contentSeparator = contentSeparators
                .stream()
                .filter(s -> sectionText.contains(s))
                .findFirst()
                .orElse(null);

        if (contentSeparator == null) {
            // it can happen when the section is not in form and we found another section. See
            // https://www.enarocanje.si/Obrazci/?id_obrazec=37402
            // where we want to get section "I.3", but we get "II.3"
            return null;
        }

        assert sectionText.contains(contentSeparator);
        return sectionText.substring(sectionText.indexOf(contentSeparator) + contentSeparator.length()).trim();
    }

    /**
     * Parses buyer type and set it to buyers.
     *
     * @param buyers
     *          list of buyers
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return list of buyers or null
     */
    static List<ParsedBody> parseBuyerType(final List<ParsedBody> buyers, final Element form) {
        final String buyerType = StringUtils.removeDotsAtTheEnd(ENarocanjeTenderFormInTableUtils.getSectionContent(
                "I.2", form));
        if (buyerType == null) {
            return buyers;
        }

        if (buyers == null) {
            return new ArrayList<>(Collections.singleton(new ParsedBody()
                    .setBuyerType(buyerType)));
        } else {
            buyers.get(0)
                    .setBuyerType(buyerType);
            return buyers;
        }
    }
}
