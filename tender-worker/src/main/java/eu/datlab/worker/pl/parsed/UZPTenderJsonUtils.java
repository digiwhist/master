package eu.datlab.worker.pl.parsed;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.MissingNode;
import org.apache.commons.lang.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * Json utils for UZP Parser.
 */
public final class UZPTenderJsonUtils {

    /**
     * Suppress default constructor.
     */
    private UZPTenderJsonUtils() {
    }

    /**
     * Attempts to get JSON node from the given path. Path composes from names and/or indexes of nodes separated by a slash.
     *
     * @param path
     *      path of the node
     * @param context
     *      context to be searched
     * @return json node or missing node
     */
    public static JsonNode path(final String path, final JsonNode context) {
        if (path == null || context == null) {
            return  MissingNode.getInstance();
        }

        JsonNode node = context;
        for (String p : path.split("/")) {
            if (node.isMissingNode()) {
                break;
            }
            node = StringUtils.isNumeric(p) ? node.path(Integer.valueOf(p)) : node.path(p);
        }

        return node;
    }

    /**
     * Attempts to get text value of the JSON node from the given path. If the JSON node is textual returns output of method
     *
     * @param path
     *      path of node
     * @param context
     *      context to be searched
     * @return string or null
     */
    public static String textValue(final String path, final JsonNode context) {
        JsonNode node = path(path, context);
        if (node.isMissingNode()) {
            return null;
        }

        return node.isTextual() ? node.textValue() : node.toString();
    }

    /**
     * Get text value of node.
     *
     * @param paths paths
     * @param context context
     * @return String
     */
    public static String textValue(final String[] paths, final JsonNode context) {
        for (String path : paths) {
            if (path != null) {
                return path;
            }
        }

        return null;
    }

    /**
     * @param node
     *      node to be parsed
     * @return list of JSON nodes of an item or empty map
     */
    public static List<JsonNode> parseItems(final JsonNode node) {
        if (node == null || node.isMissingNode()) {
            return Collections.emptyList();
        }

        final List<JsonNode> items = new ArrayList<>();

        Iterator<JsonNode> itemNodes = node.elements();
        while (itemNodes.hasNext()) {
            items.add(itemNodes.next());
        }

        return items;
    }
}
