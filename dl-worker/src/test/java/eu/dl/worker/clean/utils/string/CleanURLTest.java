package eu.dl.worker.clean.utils.string;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.net.URL;

import org.junit.Test;

import eu.dl.worker.clean.utils.StringUtils;
import eu.dl.worker.clean.utils.URLSchemeType;

/**
 * Tests of string to boolean transformation in StringUtils.
 *
 * @author Tomas Mrazek
 */
public final class CleanURLTest {
    /**
     * Test of null value.
     */
    @Test
    public void nullValue() {
        assertNull(StringUtils.cleanURL(null));
        assertNull(StringUtils.cleanURL(null, URLSchemeType.HTTP));
    }

    /**
     * Test of nice string values.
     */
    @Test
    public void okValues() {
        assertTrue(StringUtils.cleanURL("http://www.test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("www.test.com", URLSchemeType.HTTP) instanceof URL);
    }

    /**
     * Test of string values with typos.
     */
    @Test
    public void typosValues() {
        assertTrue(StringUtils.cleanURL("www:test.com", URLSchemeType.HTTP) instanceof URL);
        assertTrue(StringUtils.cleanURL("WWW:test.com", URLSchemeType.HTTP) instanceof URL);
        assertTrue(StringUtils.cleanURL("htt://www.test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("hhtts://www.test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("http//www.test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("https:/www.test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("asdfasdhttp://www.test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("hjttp://test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("http://http:www.test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("http://http:test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL(":http://www.test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("http://:www.test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("http://somethingText:http://www.test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("www.test.comhttps://www.test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("www.htp://test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("ttps://test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("https:test.com") instanceof URL);
        assertTrue(StringUtils.cleanURL("http:www.test.com") instanceof URL);
    }
    
    /**
     * Malformed values - should throw an exception.
     */
    @Test
    public void malformedTest() {
        assertNull(StringUtils.cleanURL("www.test.com"));
        assertNull(StringUtils.cleanURL("non url string"));
        assertNull(StringUtils.cleanURL("foo://www.test.com"));
        assertNull(StringUtils.cleanURL("http://"));
        assertNull(StringUtils.cleanURL("https:"));
    }
}
