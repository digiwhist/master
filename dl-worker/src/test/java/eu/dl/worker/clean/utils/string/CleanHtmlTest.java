package eu.dl.worker.clean.utils.string;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import eu.dl.worker.clean.utils.StringUtils;

/**
 * Test of clean html in String utils.
 * 
 * @author Kuba Krafka
 */
public final class CleanHtmlTest {

    /**
     * Test of null value.
     */
    @Test
    public void nullOrEmptyValue() {
        assertNull(StringUtils.cleanHtml(null));
        assertNull(StringUtils.cleanHtml(""));
        assertNull(StringUtils.cleanHtml(" "));
    }

    /**
     * Test of nice string values.
     */
    @Test
    public void okValues() {
        assertTrue(StringUtils.cleanHtml("no html").equals("no html"));
        assertTrue(StringUtils.cleanHtml("no html ").equals("no html"));
        assertTrue(StringUtils.cleanHtml("no > html").equals("no > html"));
        assertTrue(StringUtils.cleanHtml("<b>ok\n\n</b>\n").equals("ok"));
        assertTrue(StringUtils.cleanHtml(" <b> \n").equals(""));
        assertTrue(StringUtils.cleanHtml(" <b><i></b></i> ").equals(""));
        assertTrue(StringUtils.cleanHtml(" <b><i>a\n\n</body></td> ").equals("a"));
        assertTrue(StringUtils.cleanHtml(" <b><i>a\n\n<br></td> ").equals("a"));
        assertTrue(StringUtils.cleanHtml("<").equals("<"));
        assertTrue(StringUtils.cleanHtml(" <b>a < b<br> ").equals("a < b"));
        assertTrue(
                StringUtils.cleanHtml("<p>par > par1</p><p>Long sentence.</p>").equals("par > par1\n\nLong sentence."));
        assertTrue(StringUtils.cleanHtml("<p>This is some text</p><ul><li>1aaa</li><li>2bbb</li><ul><p>Other text</p>")
                .equals("This is some text\n\n\n1aaa\n2bbb\n\nOther text"));
    }
}
