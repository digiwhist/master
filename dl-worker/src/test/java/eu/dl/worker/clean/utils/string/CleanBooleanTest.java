package eu.dl.worker.clean.utils.string;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import eu.dl.worker.clean.utils.StringUtils;

/**
 * Tests of string to boolean transformation in StringUtils.
 *
 * @author Tomas Mrazek
 */
public final class CleanBooleanTest {

    /**
     * Test of null value.
     */
    @Test
    public void nullValue() {
        assertNull(StringUtils.cleanBoolean(null));
    }

    /**
     * Test of nice string values.
     */
    @Test
    public void okValues() {
        assertTrue(StringUtils.cleanBoolean("true").equals(Boolean.TRUE));
        assertTrue(StringUtils.cleanBoolean("TRUE").equals(Boolean.TRUE));

        assertTrue(StringUtils.cleanBoolean("false").equals(Boolean.FALSE));
        assertTrue(StringUtils.cleanBoolean("FALSE").equals(Boolean.FALSE));
        assertTrue(StringUtils.cleanBoolean("some text").equals(Boolean.FALSE));
    }
}
