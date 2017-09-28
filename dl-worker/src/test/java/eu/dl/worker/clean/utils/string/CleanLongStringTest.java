package eu.dl.worker.clean.utils.string;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import eu.dl.worker.clean.utils.StringUtils;

/**
 * Test of clean long string in String utils.
 * 
 * @author Kuba Krafka
 */
public final class CleanLongStringTest {

    /**
     * Test of null value.
     */
    @Test
    public void nullOrEmptyValue() {
        assertNull(StringUtils.cleanLongString(null));
        assertNull(StringUtils.cleanLongString(""));
        assertNull(StringUtils.cleanLongString(" "));
        assertNull(StringUtils.cleanLongString(" \n "));
        assertNull(StringUtils.cleanLongString(" <b> "));
        assertNull(StringUtils.cleanLongString(" <b><i></b>\n</i> "));
    }

    /**
     * Test of nice string values.
     */
    @Test
    public void okValues() {
        assertTrue(StringUtils.cleanLongString("no html").equals("no html"));
        assertTrue(StringUtils.cleanLongString("no html ").equals("no html"));
        assertTrue(StringUtils.cleanLongString("no > html").equals("no > html"));
        assertTrue(StringUtils.cleanLongString("<b>ok</b>\n\n").equals("ok"));
        assertTrue(StringUtils.cleanLongString(" <b><i>a\n</body></td> ").equals("a"));
        assertTrue(StringUtils.cleanLongString(" <b><i>a<br></td> ").equals("a"));
        assertTrue(StringUtils.cleanLongString("<").equals("<"));
        assertTrue(StringUtils.cleanLongString(" <b>a < b<br>\n\n ").equals("a < b"));
        assertTrue(
                StringUtils.cleanLongString("<p>par > par1</p><p>Long sentence.</p>")
                        .equals("par > par1\n\nLong sentence."));
        assertTrue(StringUtils
                .cleanLongString("<p>This is some text</p><ul><li>1aaa</li><li>2bbb</li><ul><p>Other text</p>")
                .equals("This is some text\n\n\n1aaa\n2bbb\n\nOther text"));
    }
}
