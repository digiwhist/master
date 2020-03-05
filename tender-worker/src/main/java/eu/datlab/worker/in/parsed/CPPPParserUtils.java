package eu.datlab.worker.in.parsed;

import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;

/**
 * Utils class for Indian parser.
 */
public final class CPPPParserUtils {

    /**
     * Suppress default constructor.
     */
    private CPPPParserUtils() {
    }

    /**
     * Parses value by label from detail table.
     *
     * @param context
     *      context to be searched
     * @param label
     *      label of field
     * @return value or null
     */
    public static String getValue(final Element context, final String label) {
        // old forms - value node is first next sibling of label node
        Element node = JsoupUtils.getNthSibling(JsoupUtils.selectFirst("*:matchesOwn(" + label + ")", context), 1);
        if (node == null) {
            return null;
        }
        // new forms - value node is second next sibling of label node, first next sibling contains ":"
        if (node.text().trim().equals(":")) {
            node = JsoupUtils.getNthSibling(node, 1);
        }

        return node == null ? null : node.text();
    }
}
