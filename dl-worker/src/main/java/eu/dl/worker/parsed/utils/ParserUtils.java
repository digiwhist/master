package eu.dl.worker.parsed.utils;

import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;
import org.jsoup.parser.Tag;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Provides useful methods for parsers.
 *
 * @author Marek Mikes
 */
public final class ParserUtils {
    /**
     * Private constructor to make this class static.
     */
    private ParserUtils() {
    }

    /**
     * Create parent element with elements of section between the two elements.
     *
     * @param firstRow
     *         first element of section
     * @param endRow
     *         element before which section ends
     *
     * @return Element
     */
    public static Element getSubsectionOfElements(final Element firstRow, final Element endRow) {
        if (firstRow != null) {
            Elements subSectionRows;
            Element parent = firstRow.parent();
            int startIndex = firstRow.elementSiblingIndex();
            if (endRow != null) {
                subSectionRows = parent.select(
                        String.format("> *:not(:lt(%d)):lt(%d)", startIndex, endRow.elementSiblingIndex()));
            } else {
                subSectionRows = parent.select(String.format("> *:not(:lt(%d))", startIndex));
            }
            return cloneAndWrapByRootElement(subSectionRows);
        }
        return null;
    }

    /**
     * Create parent element with nodes of section between the two elements. Moreover, the section nodes will be
     * separated by <br> tag because of convenience parsing
     *
     * @param firstRow
     *         first element of section
     * @param endRow
     *         element before which section ends
     *
     * @return Element
     */
    public static Element getSubsectionOfNodes(final Element firstRow, final Element endRow) {
        if (firstRow != null) {
            List<Node> subSectionRows = new ArrayList<>();
            Node node = firstRow;
            do {
                subSectionRows.add(node);
                subSectionRows.add(new Element(Tag.valueOf("br"), "")); // nodes will be separated (useful for text)
                node = node.nextSibling();
                if (node == null) {
                    break;
                }
            } while (endRow == null ? true : !node.equals(endRow));
            return cloneAndWrapByRootElement(subSectionRows);
        }
        return null;
    }

    /**
     * The method gets row from some element content.
     *
     * @param element
     *         element to parse data from
     * @param contentSelector
     *         selector which selects element content. It can select more than one element, but we want the first one
     * @param informationIndex
     *         row index in content (first is 0)
     *
     * @return row from some element content or null when the row is not filled or is "-"
     */
    public static String getFromContent(final Element element, final String contentSelector,
                                        final int informationIndex) {
        Element subsection = contentSelector == null ? element : JsoupUtils.selectFirst(contentSelector, element);
        if (subsection == null) {
            return null;
        }
        String[] subsectionRows = subsection.html().split("<br>");
        String result = informationIndex < subsectionRows.length ? subsectionRows[informationIndex].trim() : null;
        return Objects.equals("-", result) ? null : result;
    }

    /**
     * The method gets desired row information after title from some element content.
     *
     * @param element
     *         element to parse data from
     * @param contentSelector
     *         selector which selects element content. It can select more than one element, but we want the first one
     * @param informationTitle
     *         title (first part of some row which includes colon)
     *
     * @return row information after title from some element content or null when the row is not filled or is "-"
     */
    public static String getFromContent(final Element element, final String contentSelector,
                                        final String informationTitle) {
        Element subsection = contentSelector == null ? element : JsoupUtils.selectFirst(contentSelector, element);
        if (subsection == null) {
            return null;
        }
        String[] subsectionRows = subsection.html().split("<br>");
        for (String subsectionRow : subsectionRows) {
            if (subsectionRow.startsWith(informationTitle)) {
                String result = subsectionRow.substring(informationTitle.length()).trim();
                return result.equals("-") ? null : result;
            }
        }

        return null;
    }

    /**
     * Create wrapper for elements.
     *
     * @param elementsToWrap
     *         elements to be wrapped
     *
     * @return Element
     */
    private static Element cloneAndWrapByRootElement(final Elements elementsToWrap) {
        if (elementsToWrap == null) {
            return null;
        }

        // without cloning, appending to a new element would remove the elements
        // from original parent
        final Elements elementsToWrapCopy = elementsToWrap.clone();

        Element wrapper = new Element(Tag.valueOf("wrapper"), "");
        for (Element elem : elementsToWrapCopy) {
            wrapper.appendChild(elem);
        }
        return wrapper;
    }

    /**
     * Create wrapper for nodes.
     *
     * @param nodesToWrap
     *         nodes to be wrapped
     *
     * @return parent element of input nodes
     */
    private static Element cloneAndWrapByRootElement(final List<Node> nodesToWrap) {
        if (nodesToWrap == null) {
            return null;
        }

        // without cloning, appending to a new element would remove the elements
        // from original parent
        final List<Node> elementsToWrapCopy = nodesToWrap
                .stream()
                .map(n -> n.clone())
                .collect(Collectors.toList());

        Element wrapper = new Element(Tag.valueOf("wrapper"), "");
        for (Node elem : elementsToWrapCopy) {
            wrapper.appendChild(elem);
        }
        return wrapper;
    }

}
