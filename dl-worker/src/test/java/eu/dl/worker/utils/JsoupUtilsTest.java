package eu.dl.worker.utils;

import java.util.List;
import java.util.function.Function;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;
import org.junit.Assert;
import org.junit.Test;

import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 *
 * @author Tomas Mrazek
 */
public class JsoupUtilsTest {

    private static final String XML = new StringBuilder()
        .append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
        .append("<doc>")
            .append("<title>Jsoup Test</title>")

            .append("<empty></empty>")

            .append("<numbered_elements count='3'>")
                .append("<element_1>1</element_1>")
                .append("<element_2>2</element_2>")
                .append("<element_3>3</element_3>")
            .append("</numbered_elements>")

            .append("<table>")
                .append("<tr><td>First item</td><td>1</td></tr>")
                .append("<tr><td>Second item</td><td>2</td></tr>")
                .append("<tr><td>Third ITEM</td><td>3</td></tr>")
            .append("</table>")
        .append("</doc>")
    .toString();

    private Document xml;

    /**
     * Constructor with XML document initialization.
     */
    public JsoupUtilsTest() {
        xml = Jsoup.parse(XML, "", Parser.xmlParser());
    }

    /**
     * Checks the getAllValuesByLabel, getFirstValueByLabel, getFirstLabeledValueNode, and getLabeledValueNodes function
     * from the JsoupUtils class.
     */
    @Test
    public final void getByLabelTest() {
        Assert.assertTrue(JsoupUtils.getLabeledValueNodes(xml, "item").size() == 2);
        
        List<String> labeledValues = JsoupUtils.getAllValuesByLabel(xml, "(?i)item");
        Assert.assertTrue(labeledValues.size() == 3);
        Assert.assertTrue(labeledValues.get(0).equals("1"));
        Assert.assertTrue(labeledValues.get(1).equals("2"));
        Assert.assertTrue(labeledValues.get(2).equals("3"));


        Assert.assertTrue(JsoupUtils.getLabeledValueNodes(xml, "First|Second").size() == 2);
        Assert.assertNull(JsoupUtils.getFirstValueByLabel(xml, "first"));
        Assert.assertTrue(JsoupUtils.getFirstValueByLabel(xml, "(?i)first").equals("1"));        
        Assert.assertTrue(JsoupUtils.getFirstValueByLabel(xml, "First").equals("1"));
        Assert.assertNull(JsoupUtils.getFirstValueByLabel(xml, "Five"));
        Assert.assertNull(JsoupUtils.getFirstValueByLabel(null, "First"));


        Assert.assertTrue(JsoupUtils.getFirstLabeledValueNode(xml, "First").text().equals("1"));
        Assert.assertNull(JsoupUtils.getFirstLabeledValueNode(null, "First"));
        Assert.assertNull(JsoupUtils.getFirstLabeledValueNode(xml, "Five"));
    }

    /**
     * Checks the selectNumberedElementsTest function from the JsoupUtils class.
     */
    @Test
    public final void selectNumberedElementsTest() {
        Function<Integer, String> nameProducer = (i) -> "element_" + i;
        
        Assert.assertTrue(JsoupUtils.selectNumberedElements(nameProducer, xml).size() == 3);
        Assert.assertTrue(JsoupUtils.selectNumberedElements(nameProducer, xml, 2).size() == 2);
        Assert.assertTrue(JsoupUtils.selectNumberedElements(nameProducer, xml, 4).isEmpty());
        Assert.assertTrue(JsoupUtils.selectNumberedElements(nameProducer, null).isEmpty());
    }

    /**
     * Checks the exists function from the JsoupUtils class.
     */
    @Test
    public final void existsTest() {
        Assert.assertTrue(JsoupUtils.exists("doc", xml));
        Assert.assertFalse(JsoupUtils.exists("uknown", xml));
        Assert.assertFalse(JsoupUtils.exists("doc", null));
        Assert.assertTrue(JsoupUtils.exists("doc", xml, "yes", "no").equals("yes"));
        Assert.assertTrue(JsoupUtils.exists("unknown", xml, "yes", "no").equals("no"));
    }

    /**
     * Checks the hasAttribute function from the JsoupUtils class.
     */
    @Test
    public final void hasAttributeTest() {
        Element elm = xml.select("numbered_elements").first();

        Assert.assertTrue(JsoupUtils.hasAttribute(elm, "count", "3"));        
        Assert.assertFalse(JsoupUtils.hasAttribute(elm, "count", "2"));
        Assert.assertFalse(JsoupUtils.hasAttribute(elm, "unknown", "3"));
        Assert.assertFalse(JsoupUtils.hasAttribute(null, "count", "3"));
    }

    /**
     * Checks the hasText function from the JsoupUtils class.
     */
    @Test
    public final void hasTextTest() {
        Assert.assertTrue(JsoupUtils.hasText("element_1", xml, "1"));
        Assert.assertFalse(JsoupUtils.hasText("element_1", xml, "2"));
        Assert.assertFalse(JsoupUtils.hasText("element_1", null, "1"));
    }

    /**
     * Checks the selectAttribute function from the JsoupUtils class.
     */
    @Test
    public final void selectAttributeTest() {
        Element elm = xml.select("numbered_elements").first();

        Assert.assertTrue(JsoupUtils.selectAttribute("numbered_elements", "count", xml).equals("3"));
        Assert.assertTrue(JsoupUtils.selectAttribute("count", elm).equals("3"));
        Assert.assertNull(JsoupUtils.selectAttribute("numbered_elements", "count", null));
        Assert.assertNull(JsoupUtils.selectAttribute("numbered_elements", "unknown", xml));
        Assert.assertNull(JsoupUtils.selectAttribute("unknown", "unknown", xml));
    }

    /**
     * Checks the selectCombinedText function from the JsoupUtils class.
     */
    @Test
    public final void selectCombinedTextTest() {        
        Assert.assertTrue(JsoupUtils.selectCombinedText("numbered_elements", xml).equals("123"));
        Assert.assertNull(JsoupUtils.selectCombinedText("unknown", xml));
        Assert.assertNull(JsoupUtils.selectCombinedText("unknown", null));
    }

    /**
     * Checks the selectTagName function from the JsoupUtils class.
     */
    @Test
    public final void selectTagNameTest() {
        Assert.assertTrue(JsoupUtils.selectTagName("doc", xml).equals("DOC"));
        Assert.assertNull(JsoupUtils.selectTagName("unknown", xml));
        Assert.assertNull(JsoupUtils.selectTagName("unknown", null));
    }

    /**
     * Checks the selectText function from the JsoupUtils class.
     */
    @Test
    public final void selectTextTest() {
        Assert.assertTrue(JsoupUtils.selectText("table > tr > td", xml).equals("First item"));
        Assert.assertTrue(JsoupUtils.selectText("unknown", xml, true).equals(""));
        Assert.assertNull(JsoupUtils.selectText("unknown", xml));
    }

    /**
     * Checks the select and selectFirst function from the JsoupUtils class.
     */
    @Test
    public final void selectTest() {
        Assert.assertTrue(JsoupUtils.select("numbered_elements > *", xml).size() == 3);
        Assert.assertTrue(JsoupUtils.selectFirst("numbered_elements > *", xml).text().equals("1"));
        Assert.assertTrue(JsoupUtils.select("unknown", xml).isEmpty());
        Assert.assertNull(JsoupUtils.select("table", null));
    }

    /**
     * Checks the getRoot function from the JsoupUtils class.
     */
    @Test
    public final void getRootTest() {
        Assert.assertTrue(JsoupUtils.getRoot(xml).tagName().equals("doc"));
        Assert.assertNull(JsoupUtils.getRoot(null));
    }
}
