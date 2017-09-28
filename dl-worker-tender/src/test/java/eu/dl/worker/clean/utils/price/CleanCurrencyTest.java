package eu.dl.worker.clean.utils.price;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Currency;

import org.junit.Test;

import eu.dl.worker.clean.utils.PriceUtils;

/**
 * Test of clean html in String utils.
 *
 * @author Tomas Mrazek
 */
public final class CleanCurrencyTest {

    /**
     * Test of null value.
     */
    @Test
    public void nullOrEmptyValue() {
        assertNull(PriceUtils.cleanPriceCurrency(null));
        assertNull(PriceUtils.cleanPriceCurrency(""));
        assertNull(PriceUtils.cleanPriceCurrency(" "));
    }

    /**
     * Test of correct ISO 4217 codes.
     */
    @Test
    public void okValues() {
        assertTrue(PriceUtils.cleanPriceCurrency("EUR") instanceof Currency);
        assertTrue(PriceUtils.cleanPriceCurrency("CZK") instanceof Currency);
        assertTrue(PriceUtils.cleanPriceCurrency("USD") instanceof Currency);
    }

    /**
     * Test malformed values - should throw an exception.
     */
    @Test
    public void wrongValues() {
        assertNull(PriceUtils.cleanPriceCurrency("no ISO 4217"));
        assertNull(PriceUtils.cleanPriceCurrency("CKZ"));
    }
}
