package eu.datlab.worker.hr.parsed;

import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;

/**
 * Utilities for Croatian forms.
 *
 * @author Marek Mikes
 */
final class EOJNTenderFormUtils {
    static final String PUBLICATION_SOURCE_FORM_TYPE_OLD_SELECTOR = "div > span.eu_uppercase";

    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderFormUtils() {
    }

    /**
     * Form ages.
     */
    public enum FormAge {
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
        if (JsoupUtils.exists(PUBLICATION_SOURCE_FORM_TYPE_OLD_SELECTOR, document)) {
            return FormAge.OLD;
        } else {
            return FormAge.NEW;
        }
    }

}
