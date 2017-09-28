package eu.dl.worker.clean.utils.number;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.text.NumberFormat;
import java.util.Locale;

import org.junit.Test;

import eu.dl.worker.clean.utils.NumberUtils;

/**
 * @author Kuba Krafka
 */
public final class CleanIntegerTest {

    private final NumberFormat numberFormat = NumberFormat.getInstance(new Locale("en_US"));

    /**
     * Test of null value.
     */
    @Test
    public void nullValue() {
        assertNull(NumberUtils.cleanInteger(null, numberFormat));
        assertNull(NumberUtils.cleanInteger("", numberFormat));
        assertNull(NumberUtils.cleanInteger(" ", numberFormat));
    }

    /**
     * Test of nice string values.
     */
    @Test
    public void okValues() {
        assertTrue(NumberUtils.cleanInteger("1000", numberFormat) == 1000);
        assertTrue(NumberUtils.cleanInteger("1000.00", numberFormat) == 1000);
        assertTrue(NumberUtils.cleanInteger("1000 00", numberFormat) == 1000);
        assertTrue(NumberUtils.cleanInteger("1000 d", numberFormat) == 1000);
        assertTrue(NumberUtils.cleanInteger("1000 d", numberFormat) == 1000);
        assertTrue(NumberUtils.cleanInteger("1000 d", numberFormat) == 1000);
        assertTrue(NumberUtils.cleanInteger("10aa00", numberFormat) == 10);
        assertTrue(NumberUtils.cleanInteger("1,000", numberFormat) == 1000);
        assertTrue(NumberUtils.cleanInteger("1,000.500", numberFormat) == 1000);
        assertTrue(NumberUtils.cleanInteger("1,000.", numberFormat) == 1000);
        assertTrue(NumberUtils.cleanInteger("1,000 12,1", numberFormat) == 1000);
        assertTrue(NumberUtils.cleanInteger("1.000", numberFormat) == 1);
        assertTrue(NumberUtils.cleanInteger("1 000", numberFormat) == 1);
        assertTrue(NumberUtils.cleanInteger("0.500", numberFormat) == 0);
        assertTrue(NumberUtils.cleanInteger("no 100df0 d", numberFormat).equals(100));
        assertTrue(NumberUtils.cleanInteger("no100", numberFormat).equals(100));
        assertTrue(NumberUtils.cleanInteger("100%", numberFormat).equals(100));
    }
}
