package eu.digiwhist.worker.hr.parsed;

import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;

import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SECTION_SELECTOR_PATTERN;

/**
 * Utilities for Croatian old forms.
 *
 * @author Marek Mikes
 */
final class EOJNTenderOldFormUtils {
    static final String SUBSECTION_II_1_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "II.1");
    static final String SUBSECTION_II_2_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "II.2");
    static final String SUBSECTION_II_3_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "II.3");

    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderOldFormUtils() {
    }

    /**
     * Parse tender selection method value from document.
     *
     * @param subsectionIV2Element
     *         subsection IV.2 to be parsed
     *
     * @return String or Null
     */
    static String parseTenderSelectionMethod(final Element subsectionIV2Element) {
        // the subsection contains other checkboxes, so we can not use selector to get general checkbox
        final String selectionMethod = JsoupUtils.selectText(
                "td:has(p > input[name=NajnizaCijena1][checked]) + td," +
                "td:has(p > input[name=EkNajpPon1][checked]) + td",
                subsectionIV2Element);

        // the method can be unfilled.
        // See https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=203989

        return selectionMethod;
    }

    /**
     * Parse tender description value from document.
     *
     * @param subsection
     *         subsection to be parsed
     * @param title
     *         title or part of title of description
     *
     * @return String or Null
     */
    static String parseTenderDescription(final Element subsection, final String title) {
        return JsoupUtils.selectCombinedText("p:contains(" + title + ") ~ p", subsection);
    }

}
