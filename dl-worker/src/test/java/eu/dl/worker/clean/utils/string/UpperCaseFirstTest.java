package eu.dl.worker.clean.utils.string;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import eu.dl.worker.clean.utils.StringUtils;

/**
 * Tests of upper case method in StringUtils.
 * 
 * @author Kuba Krafka
 */
public final class UpperCaseFirstTest {

    /**
     * Test of null value.
     */
    @Test
    public void nullValue() {
        assertNull(StringUtils.upperCaseFirst(null));
    }

    /**
     * Test of nice string values.
     */
    @Test
    public void okValues() {
        assertTrue(StringUtils.upperCaseFirst("no html").equals("No html"));
        assertTrue(StringUtils.upperCaseFirst("No html ").equals("No html "));
        assertTrue(StringUtils.upperCaseFirst("<a>html ").equals("<a>html "));
        assertTrue(StringUtils.upperCaseFirst(" ").equals(" "));
        assertTrue(StringUtils.upperCaseFirst("UPPER").equals("UPPER"));
    }
}
