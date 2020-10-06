package eu.dl.worker.clean.utils.string;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.worker.clean.utils.BodyIdentifierUtils;
import eu.dl.worker.clean.utils.StringUtils;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

/**
 * Test of clean body identifiers in String utils.
 *
 * @author Marek Mikes
 */
public final class CleanBodyIdentifierTest {
    /**
     * Test of null value.
     */
    @Test
    public void nullValues() {
        assertNull(StringUtils.cleanBodyIdentifier(null));
        assertNull(StringUtils.cleanBodyIdentifier(""));
        assertNull(StringUtils.cleanBodyIdentifier(" "));
        assertNull(StringUtils.cleanBodyIdentifier("     "));
        assertNull(StringUtils.cleanBodyIdentifier("-"));
        assertNull(StringUtils.cleanBodyIdentifier("1"));
        assertNull(StringUtils.cleanBodyIdentifier("12"));
        assertNull(StringUtils.cleanBodyIdentifier(" 1 2 "));
        assertNull(StringUtils.cleanBodyIdentifier("01234567890123456789012345678901234567890123456789" +
                "012345678901234567890123456789012345678901234567890"));

        assertNull(BodyIdentifierUtils.cleanBodyIdentifier(new BodyIdentifier().setId("12").setScope(BodyIdentifier.Scope.CZ)).getId());
    }

    /**
     * Test of nice string values.
     */
    @Test
    public void okValues() {
        assertTrue(StringUtils.cleanBodyIdentifier("123").equals("123"));
        assertTrue(StringUtils.cleanBodyIdentifier(" 1 2 3 ").equals("123"));
        assertTrue(StringUtils.cleanBodyIdentifier(" 123 ").equals("123"));
        assertTrue(StringUtils.cleanBodyIdentifier("1 2 3").equals("123"));
        assertTrue(StringUtils.cleanBodyIdentifier("someText").equals("someText"));
        assertTrue(StringUtils.cleanBodyIdentifier("12345").equals("12345"));

        assertTrue(StringUtils.cleanBodyIdentifier("a", true).equals("a"));
        assertTrue(StringUtils.cleanBodyIdentifier(" a", true).equals("a"));
        assertTrue(StringUtils.cleanBodyIdentifier("12", true).equals("12"));
        assertTrue(StringUtils.cleanBodyIdentifier("1 2 ", true).equals("12"));

        assertEquals("12", BodyIdentifierUtils.cleanBodyIdentifier(new BodyIdentifier().setId("12")
                .setScope(BodyIdentifier.Scope.UG)).getId());
    }
}
