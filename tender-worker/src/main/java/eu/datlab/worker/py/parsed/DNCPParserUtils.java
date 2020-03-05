package eu.datlab.worker.py.parsed;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.MissingNode;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedDocument;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import eu.dl.dataaccess.dto.parsed.ParsedUnitPrice;
import org.apache.commons.collections.IteratorUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * DNCP parser utils.
 *
 * @author Tomas Mrazek
 */
public final class DNCPParserUtils {
    private static final Logger logger = LoggerFactory.getLogger(DNCPParserUtils.class.getName());

    /**
     * Suppress default constructor.
     */
    private DNCPParserUtils() {
    }

    /**
     * @param node
     *      node to be parsed
     * @return non-empty list of praed lots or null
     */
    public static List<ParsedTenderLot> parseLots(final JsonNode node) {
        if (node == null || node.isMissingNode()) {
            return null;
        }

        Map<String, JsonNode> items = parseItems(node);

        List<ParsedTenderLot> lots = new ArrayList<>();

        Iterator<JsonNode> lotIds = path("lots/0/items", node).elements();
        while (lotIds.hasNext()) {
            String id = lotIds.next().textValue();

            JsonNode i = items.get(id);
            if (i != null) {
                lots.add(new ParsedTenderLot()
                    .setTitle(textValue("description", i))
                    .addCpv(new ParsedCPV()
                        .setCode(textValue("classification/id", i)))
                    .addBid(new ParsedBid()
                        .setIsWinning(Boolean.TRUE.toString())
                        .setPrice(parsePrice(path("value", i))))
                );
            } else {
                logger.warn("Item with id {} for lot not found.", id);
            }
        }

        return lots.isEmpty() ? null : lots;

    }

    /**
     * @param node
     *      node to be parsed
     * @return non-empty list of parsed documents or null
     */
    public static List<ParsedDocument> parseDocuments(final JsonNode node) {
        if (node == null || node.isMissingNode()) {
            return null;
        }

        Iterator<JsonNode> documentNodes = node.elements();
        List<ParsedDocument> documents = new ArrayList<>();
        while (documentNodes.hasNext()) {
            JsonNode n = documentNodes.next();
            documents.add(new ParsedDocument()
                .setDescription(textValue("documentType", n))
                .setTitle(textValue("title", n))
                .setLanguage(textValue("language", n))
                .setUrl(textValue("url", n))
                .setFormat(textValue("format", n))
            );
        }

        return documents.isEmpty() ? null : documents;
    }

    /**
     * @param node
     *      node to be parsed
     * @return parsed price or null
     */
    public static ParsedPrice parsePrice(final JsonNode node) {
        if (node == null || node.isMissingNode()) {
            return null;
        }

        return new ParsedPrice()
            .setNetAmount(textValue("amount", node))
            .setCurrency(textValue("currency", node));
    }

    /**
     * @param node
     *      node to be parsed
     * @return map of item id and JSON node of an item or empty map
     */
    public static Map<String, JsonNode> parseItems(final JsonNode node) {
        if (node == null || node.isMissingNode()) {
            return Collections.emptyMap();
        }

        Map<String, JsonNode> items = new HashMap<>();

        Iterator<JsonNode> itemNodes = path("items", node).elements();
        while (itemNodes.hasNext()) {
            JsonNode n = itemNodes.next();
            items.put(textValue("id", n), n);
        }

        return items;
    }

    /**
     * @param node
     *      node to be parsed
     * @return parsed body or null
     */
    public static ParsedBody parseBody(final JsonNode node) {
        if (node == null || node.isMissingNode()) {
            return null;
        }

        JsonNode contactPoint = path("contactPoint", node);

        ParsedAddress address = parseAddress(path("address", node));
        if (address != null) {
            address.setUrl(textValue("url", contactPoint));
        }

        return new ParsedBody()
            .setName(textValue("name", node))
            .addBodyId(parseBodyId(path("identifier", node)))
            .setEmail(textValue("email", contactPoint))
            .setPhone(textValue("telephone", contactPoint))
            .setContactName(textValue("name", contactPoint))
            .setAddress(address);
    }

    /**
     * @param node
     *      node to be parsed
     * @return parsed address or null
     */
    public static ParsedAddress parseAddress(final JsonNode node) {
        if (node == null || node.isMissingNode()) {
            return null;
        }
        
        return new ParsedAddress()
            .setStreet(textValue("streetAddress", node))
            .setPostcode(textValue("postalCode", node))
            .setCountry(textValue("countryName", node))
            .setCity(textValue("locality", node));
    }

    /**
     * @param node
     *      node to be parsed
     * @return parsed body id or null
     */
    public static BodyIdentifier parseBodyId(final JsonNode node) {
        if (node == null || node.isMissingNode()) {
            return null;
        }

        return new BodyIdentifier()
            .setId(textValue("id", node))
            .setScope(BodyIdentifier.Scope.PY)
            .setType(BodyIdentifier.Type.ORGANIZATION_ID);
    }

    /**
     * @param <T>
     *      class of result list items
     * @param nodes
     *      nodes to be parsed
     * @param parser
     *      function that parses item of class T from JsonNode
     * @return return non-empty list of non-null items of class T or null
     */
    public static <T> List<T> parseList(final Iterator nodes, final Function<JsonNode, T> parser) {
        List<T> result = ((List<JsonNode>) IteratorUtils.toList(nodes))
            .stream()
            .map(parser::apply)
            .filter(Objects::nonNull)
            .collect(Collectors.toList());

        return result.isEmpty() ? null : result;
    }

    /**
     * Attempts to get JSON node from the given path. Path composes from names and/or indexes of nodes separated by a slash.
     *
     * @see com.fasterxml.jackson.databind.JsonNode#path(java.lang.String)
     * @see com.fasterxml.jackson.databind.JsonNode#path(int)
     * @see com.fasterxml.jackson.databind.JsonNode#isMissingNode()
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
     * {@link com.fasterxml.jackson.databind.JsonNode#textValue()}, otherwise {@link com.fasterxml.jackson.databind.JsonNode#toString()}.
     *
     * @see DNCPParserUtils#path(java.lang.String, com.fasterxml.jackson.databind.JsonNode)
     * @see com.fasterxml.jackson.databind.JsonNode#textValue()
     * @see com.fasterxml.jackson.databind.JsonNode#toString()
     * @see com.fasterxml.jackson.databind.JsonNode#isTextual()
     *
     * @param path
     *      path of node
     * @param context
     *      context to be searched
     * @return string or null
     */
    public static String textValue(final String path, final JsonNode context) {
        JsonNode node = path(path, context);
        if (node.isMissingNode() || node.isNull()) {
            return null;
        }

        return node.isTextual() ? node.textValue() : node.toString();
    }

    /**
     * @param type
     *      form type name
     * @param publicationDate
     *      publication date
     * @param url
     *      human readable url
     * @param data
     *      source JSON
     * @return included publication
     */
    public static ParsedPublication parseIncludedPublication(final String type, final String publicationDate, final String url,
                                                             final JsonNode data) {
        String sourceId = textValue("id", data);

        return new ParsedPublication()
            .setSource(PublicationSources.PY_DNCP)
            .setSourceId(sourceId)
            .setSourceTenderId(getSourceTenderId(sourceId))
            .setSourceFormType(type)
            .setPublicationDate(publicationDate)
            .setIsIncluded(true)
            .setMachineReadableUrl(url);
    }

    /**
     * @param id
     *      DNCP record id (eg. 199211-laboratorio-productos-eticos-c-e-i-s-a-34)
     * @return substring from index 0 to first occurrence of '-' or null (for mentioned example above returns 199211)
     */
    public static String getSourceTenderId(final String id) {
        if (id == null) {
            return null;

        }
        return id.substring(0, id.indexOf("-"));
    }

    /**
     * @param node
     *      node to be parsed
     * @return parsed unit price or null
     */
    public static ParsedUnitPrice parseUnitPrice(final JsonNode node) {
        if (node == null || node.isMissingNode()) {
            return null;
        }

        return new ParsedUnitPrice()
            .setDescription(textValue("description", node) + ": " + textValue("classification/id", node))
            .setUnitNumber(textValue("quantity", node))
            .setUnitType(textValue("unit/name", node))
            .setNetAmount(textValue("unit/value/amount", node))
            .setCurrency(textValue("unit/value/currency", node));
    }

    /**
     * Attempts to parse endDate if exists, otherwise parses startDate.
     *
     * @param path
     *      path to the period node (usually includes nodes endDate as startDate)
     * @param context
     *      context where the deadline node is searched
     * @return deadline or null
     */
    public static String parseDeadline(final String path, final JsonNode context) {
        if (path == null || context == null) {
            return null;

        }

        String deadline = textValue(StringUtils.stripEnd(path, "/") + "/endDate", context);
        if (deadline == null) {
            deadline = textValue(StringUtils.stripEnd(path, "/") + "/startDate", context);
        }

        return deadline;
    }
}
