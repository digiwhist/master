package eu.digiwhist.worker.hr.parsed;

import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

/**
 * Utilities for Croatian old and new forms.
 *
 * @author Marek Mikes
 */
final class EOJNTenderOldAndNewFormUtils {
    // the pattern can not be "p:has(span:containsOwn(%s)) + table", because sometimes the text is in two spans. See
    // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=176427 (VI.3)
    static final String SECTION_SELECTOR_PATTERN = "p:contains(%s) + table";
    static final String SUBSECTION_I_1_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "I.1");
    static final String SUBSECTION_I_2_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "I.2");
    static final String SUBSECTION_I_3_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "I.3");
    static final String SUBSECTION_I_4_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "I.4");
    static final String SUBSECTION_I_5_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "I.5");
    static final String SUBSECTION_I_6_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "I.6");
    static final String SUBSECTION_III_1_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "III.1");
    static final String SUBSECTION_III_2_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "III.2");
    static final String SUBSECTION_IV_1_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "IV.1");
    static final String SUBSECTION_IV_2_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "IV.2");
    static final String SUBSECTION_IV_3_SELECTOR = String.format(SECTION_SELECTOR_PATTERN, "IV.3");
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
    private EOJNTenderOldAndNewFormUtils() {
    }

    /**
     * Parses source form type of publication.
     *
     * @param document
     *         parsed document
     *
     * @return publication source form type element
     */
    static String parsePublicationSourceFormType(final Document document) {
        Element sourceFormTypeElement = JsoupUtils.selectFirst(
                "p[style*='text-align:right']:has(span[style*='font-weight:bold'])", document);
        if (sourceFormTypeElement == null) {
            // e.g. https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1126476 or
            // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=1111459
            sourceFormTypeElement = JsoupUtils.selectFirst(
                    "html > body > div:first-child + br + div > p:has(br:first-child)", document);
        }
        if (sourceFormTypeElement == null) {
            // e.g. https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=336535
            sourceFormTypeElement = JsoupUtils.selectFirst("html > body > div:first-child > p:first-child", document);
        }
        assert sourceFormTypeElement != null;
        return sourceFormTypeElement.text().trim();
    }

    /**
     * Parses source ID value of publication from document.
     *
     * @param document
     *         parsed document tree for the source HTML page
     *
     * @return String or Null
     */
    static String parsePublicationSourceId(final Document document) {
        final String sourceIdTitle = "Broj objave:";
        // the Id is in span, but sometime it is in two spans, so we want parent paragraph "p". See
        // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=764694
        final Elements sourceIdElements = JsoupUtils.select(
                "html > body > div > p:contains(" + sourceIdTitle + ")", document);
        if (sourceIdElements.isEmpty()) {
            // some forms do not have the ID. See
            // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=186022
            return null;
        } else {
            // there can be many elements with "Broj objave:", but we want the last one. See
            // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=436666
            final String sourceIdParagraphText = sourceIdElements.last().text();
            if (sourceIdParagraphText.contains(sourceIdTitle)) {
                return sourceIdParagraphText.substring(sourceIdParagraphText.lastIndexOf(sourceIdTitle) +
                        sourceIdTitle.length()).trim();
            } else {
                // sometimes we find useless text, where the title of source ID is ambiguous. See
                // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=173225
                return null;
            }
        }
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
    s     * @return boolean in string or null when no checkbox is selected
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
            return isYesChecked.toString();
        } else if (isNoChecked) {
            return isNoChecked.toString();
        } else {
            return null;
        }
    }

    /**
     * Parse if tender is covered by GPA.
     *
     * @param subsection
     *         subsection to be parsed
     *
     * @return String or Null
     */
    static String parseIsTenderCoveredByGpa(final Element subsection) {
        return parseBooleanFromCheckboxes("WTOSporJNabava_DA1", "WTOSporJNabava_NE1", subsection);
    }

    /**
     * Parse if tender has lots value from document.
     *
     * @param subsectionII1
     *         subsection II.1 to be parsed
     *
     * @return String or Null
     */
    static String parseIfTenderHasLots(final Element subsectionII1) {
        return parseBooleanFromCheckboxes("GrupeDijPredNab_DA1", "GrupeDijPredNab_NE1", subsectionII1);
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
        final String name = JsoupUtils.selectText("span:containsOwn(" + namePrefix + ")", subsection);
        assert name != null && !name.isEmpty();
        return name.substring(namePrefix.length());
    }

}
