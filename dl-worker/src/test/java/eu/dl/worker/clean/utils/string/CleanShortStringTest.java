package eu.dl.worker.clean.utils.string;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import eu.dl.worker.clean.utils.StringUtils;

/**
 * Test of clean short string in String utils.
 * 
 * @author Kuba Krafka
 */
public final class CleanShortStringTest {

    /**
     * Test of null value.
     */
    @Test
    public void nullOrEmptyValues() {
        assertNull(StringUtils.cleanShortString(null));
        assertNull(StringUtils.cleanShortString(""));
        assertNull(StringUtils.cleanShortString(" <b> "));
        assertNull(StringUtils.cleanShortString(" <b><i></b>\n</i> "));
    }

    /**
     * Test of nice string values.
     */
    @Test
    public void okValues() {
        assertTrue(StringUtils.cleanShortString("no html").equals("no html"));
        assertTrue(StringUtils.cleanShortString("no html ").equals("no html"));
        assertTrue(StringUtils.cleanShortString("no > html").equals("no > html"));
        assertTrue(StringUtils.cleanShortString("<b>ok</b>\n\n").equals("ok"));
        assertTrue(StringUtils.cleanShortString(" <b><i>a\n</body></td> ").equals("a"));
        assertTrue(StringUtils.cleanShortString(" <b><i>a<br></td> ").equals("a"));
        assertTrue(StringUtils.cleanShortString("<").equals("<"));
        assertTrue(StringUtils.cleanShortString(" <b>a < b<br>\n\n ").equals("a < b"));
        assertTrue(
                StringUtils.cleanShortString("<p>par > par1</p><p>Long sentence.</p>")
                        .equals("par > par1 Long sentence."));
        assertTrue(StringUtils
                .cleanShortString("<p>This is some text</p><ul><li>1aaa</li><li>2bbb</li><ul><p>Other text</p>")
                .equals("This is some text 1aaa 2bbb Other text"));
    }
}
