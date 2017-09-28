package eu.digiwhist.worker.nl.parsed;

import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;

import static eu.digiwhist.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_I_1_CONTENT_SELECTOR;

/**
 * Utilities for TenderNed.
 *
 * @author Marek Mikes
 */
final class TenderNedTenderFormUtils {
    /**
     * Private constructor to make this class static.
     */
    private TenderNedTenderFormUtils() {
    }

    /**
     * Form ages.
     */
    public enum FormAge {
        ANCIENT,
        OLD,
        NEW,
    }

    /**
     * The method decides how old the input form is.
     *
     * @param form
     *         document to parse data from
     *
     * @return form age
     */
    static FormAge getFormAge(final Element form) {
        if (JsoupUtils.selectFirst(ANCIENT_SUBSECTION_I_1_CONTENT_SELECTOR, form) != null) {
            return FormAge.ANCIENT;
        } else if (JsoupUtils.selectFirst("h3[id='detail-publicatie:linkS1'] ~ h4:containsOwn(I.1):has(span" +
                ":containsOwn(Naam, adressen en contactpunt\\(en\\)))", form) != null) {
            return FormAge.OLD;
        } else {
            return FormAge.NEW;
        }
    }

    /**
     * Decides whether input string represents "yes" or "no" in Dutch.
     *
     * @param yesOrNoString
     *         string which represents "yes" or "no" in Dutch
     *
     * @return true when input string represents "yes" in Dutch;
     *         false when input string represents "no" in Dutch;
     *         otherwise null
     */
    static Boolean meansYes(final String yesOrNoString) {
        if (yesOrNoString == null) {
            return null;
        }

        switch (yesOrNoString.trim()) {
            case "neen":
                return false;
            case "ja":
                return true;
            default:
                // sometimes form contains nonsense as subsection IV.2.2 in
                // https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
                // a50fa403ff28449140afb4a241d2b741/pageId/D909A/huidigemenu/aankondigingen/cid/954946/cvp/join
                return null;
        }
    }

}
