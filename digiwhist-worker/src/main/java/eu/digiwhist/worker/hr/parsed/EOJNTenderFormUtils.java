package eu.digiwhist.worker.hr.parsed;

import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;

/**
 * Utilities for Croatian forms.
 *
 * @author Marek Mikes
 */
final class EOJNTenderFormUtils {
    static final String PUBLICATION_SOURCE_FORM_TYPE_ANCIENT_SELECTOR = "div > span.eu_uppercase";

    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderFormUtils() {
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
     * @param document
     *         document to parse data from
     *
     * @return form age
     */
    static FormAge getFormAge(final Document document) {
        // we can not use buyer name selector to distinguish old and new form. See the two old forms, which have the
        // name in different elements:
        // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=203989
        // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=779675

        // we can not use number of body children to distinguish old and new form. See the two old forms, which have
        // different number of body children:
        // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=385857 (many children)
        // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=787562 (one child)
        // So we distinguish then according to number of body children, which are div.

        final int numberOfDivChildrenOfBody = JsoupUtils.select("html > body > div", document).size();
        if (JsoupUtils.exists(PUBLICATION_SOURCE_FORM_TYPE_ANCIENT_SELECTOR, document)) {
            return FormAge.ANCIENT;
        } else if (numberOfDivChildrenOfBody <= 1) {
            return FormAge.OLD;
        } else {
            return FormAge.NEW;
        }
    }

}
