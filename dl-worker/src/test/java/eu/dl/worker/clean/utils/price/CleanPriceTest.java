package eu.dl.worker.clean.utils.price;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.math.BigDecimal;
import java.text.DecimalFormat;

import org.junit.Test;

import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.worker.clean.utils.PriceUtils;

/**
 * Test of clean price in Price utils.
 *
 * @author Tomas Mrazek
 */
public final class CleanPriceTest {

    /**
     * Test of null value.
     */
    @Test
    public void nullOrEmptyValue() {
        assertNull(PriceUtils.cleanPrice(null, DecimalFormat.getInstance()));
        assertNull(PriceUtils.cleanPriceCurrency(""));
        assertNull(PriceUtils.cleanPriceCurrency(" "));
    }

    /**
     * Test of correct ISO 4217 codes.
     */
    @Test
    public void okValues() {
        ParsedPrice parsedPrice = new ParsedPrice();
        parsedPrice.setAmountWithVat("121");
        parsedPrice.setMinAmountWithVat("121");
        parsedPrice.setMaxAmountWithVat("121");
        parsedPrice.setVat("21");
        parsedPrice.setCurrency("EUR");
        Price price = PriceUtils.cleanPrice(parsedPrice, DecimalFormat.getInstance());
        assertEquals(0, price.getNetAmount().compareTo(new BigDecimal(100)));
        assertEquals(0, price.getMinNetAmount().compareTo(new BigDecimal(100)));
        assertEquals(0, price.getMaxNetAmount().compareTo(new BigDecimal(100)));
        assertEquals(0, price.getNetAmountEur().compareTo(new BigDecimal(100)));     
    }
}
