package eu.datlab.worker.hr.parsed;

import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;

/**
 * Utilities for Croatian new forms.
 *
 * @author Marek Mikes
 */
final class EOJNTenderNewFormUtils {
    // the pattern can not be "p:has(span:containsOwn(%s)) + table", because sometimes the text is in two spans. See
    // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=176427 (VI.3)
    static final String SECTION_SELECTOR_PATTERN = "p:contains(%s) + table";
    static final String SUBSECTION_I_2_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "I.2");
    static final String SUBSECTION_I_3_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "I.3");
    static final String SUBSECTION_I_4_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "I.4");
    static final String SUBSECTION_I_5_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "I.5");
    static final String SUBSECTION_I_6_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "I.6");
    static final String SUBSECTION_II_1_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "II.1") + ","
            + "p:contains(II.1) + p + table";
    static final String SUBSECTION_II_2_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "II.2") + ","
            + "table:has(> tbody > tr:first-child > td > p:contains(II.2)), tr:has(td > p:contains(II.2))";
    static final String SUBSECTION_II_3_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "II.3");
    static final String SUBSECTION_III_1_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "III.1");
    static final String SUBSECTION_III_2_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "III.2");
    static final String SUBSECTION_IV_1_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "IV.1");
    static final String SUBSECTION_IV_2_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "IV.2");
    static final String SUBSECTION_IV_3_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "IV.3");
    static final String SECTION_V_SELECTOR = "p:contains(Odjeljak V: Dodjela ugovora) + p + table";
    static final String SECTION_V_SELECTOR_ALTERNATIVE = "p:contains(Odjeljak V: Sklapanje ugovora) + table";
    static final String SUBSECTION_VI_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "Odjeljak VI");
    // "VI. 3" is for example here:
    // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=385859
    static final String SUBSECTION_VI_3_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "VI.3") + "," +
            String.format(SECTION_SELECTOR_PATTERN, "VI. 3");
    static final String SUBSECTION_VI_4_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "VI.4");

    static final String TEXT_AFTER_TITLE_SELECTOR_PATTERN = "p:contains(%s) + p";
    static final String CHECKBOX_TEXT_SELECTOR = "td:has(p > input[checked]) + td";

    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderNewFormUtils() {
    }

    /**
     * Parses boolean value from two checkboxes. First checkbox represents option "yes" and second one represents "no".
     *
     * @param yesCheckboxName
     *         name of checkbox representing "yes" option
     * @param noCheckboxName
     *         name of checkbox representing "no" option
     * @param subsection
     *         subsection to be parsed
     *
     * @return boolean in string or null when no checkbox is selected
     */
    static String parseBooleanFromCheckboxes(final String yesCheckboxName, final String noCheckboxName,
                                             final Element subsection) {
        final String checkedCheckboxSelectorPattern = "input[name=%s][checked]";
        final Boolean isYesChecked = JsoupUtils.exists(String.format(checkedCheckboxSelectorPattern, yesCheckboxName),
                subsection);
        final Boolean isNoChecked = JsoupUtils.exists(String.format(checkedCheckboxSelectorPattern, noCheckboxName),
                subsection);
        if (isYesChecked) {
            assert !isNoChecked;
            return Boolean.TRUE.toString();
        } else if (isNoChecked) {
            return Boolean.FALSE.toString();
        } else {
            return null;
        }
    }

    /**
     * Parse tender appeal body name value from subsection.
     *
     * @param subsection
     *         subsection to be parsed
     *
     * @return String or Null
     */
    static String parseTenderAppealBodyName(final Element subsection) {
        final String namePrefix = "Službeni naziv:";
        // the selector can not be "span:containsOwn(" + namePrefix + ")", because sometimes the text is in two spans.
        // See https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1147039
        final String name = JsoupUtils.selectText("p:contains(" + namePrefix + ")", subsection);
        
        return name == null ? null : name.substring(namePrefix.length());
    }

}
