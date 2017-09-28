package eu.dl.worker.clean.utils.number;

import eu.dl.worker.clean.utils.NumberUtils;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 * @author Tomas Mrazek
 */
public final class CleanBigDecimalTest {

    private final NumberFormat numberFormat = NumberFormat.getInstance(new Locale("en_US"));

    /**
     * Test of null value.
     */
    @Test
    public void nullValue() {
        assertNull(NumberUtils.cleanBigDecimal(null, numberFormat));
        assertNull(NumberUtils.cleanBigDecimal("", numberFormat));
        assertNull(NumberUtils.cleanBigDecimal(" ", numberFormat));
    }

    /**
     * Test of nice string values.
     */
    @Test
    public void okValues() {
        assertTrue(NumberUtils.cleanBigDecimal("1000", numberFormat).equals(new BigDecimal(1000)));
        assertTrue(NumberUtils.cleanBigDecimal("1000.00", numberFormat).equals(new BigDecimal(1000)));
        assertTrue(NumberUtils.cleanBigDecimal("1000 00", numberFormat).equals(new BigDecimal(1000)));
        assertTrue(NumberUtils.cleanBigDecimal("1000 d", numberFormat).equals(new BigDecimal(1000)));
        assertTrue(NumberUtils.cleanBigDecimal("10aa00", numberFormat).equals(new BigDecimal(10)));
        assertTrue(NumberUtils.cleanBigDecimal("1,000", numberFormat).equals(new BigDecimal(1000)));
        assertTrue(NumberUtils.cleanBigDecimal("1,000.500", numberFormat).equals(new BigDecimal(1000.5)));
        assertTrue(NumberUtils.cleanBigDecimal("1,000.", numberFormat).equals(new BigDecimal(1000)));
        assertTrue(NumberUtils.cleanBigDecimal("1,000 12,1", numberFormat).equals(new BigDecimal(1000)));
        assertTrue(NumberUtils.cleanBigDecimal("1.000", numberFormat).equals(new BigDecimal(1)));
        assertTrue(NumberUtils.cleanBigDecimal("1 000", numberFormat).equals(new BigDecimal(1)));
        assertTrue(NumberUtils.cleanBigDecimal("0.500", numberFormat).equals(new BigDecimal(0.5)));
        assertTrue(NumberUtils.cleanBigDecimal("no 100df0 d", numberFormat).equals(new BigDecimal(100)));
        assertTrue(NumberUtils.cleanBigDecimal("no100", numberFormat).equals(new BigDecimal(100)));
        assertTrue(NumberUtils.cleanBigDecimal("40%", numberFormat).equals(new BigDecimal(40)));
    }
    
    /**
     * Test of nice strings parsed by the set of number formats. Best parsing result should by selected.
     */
    @Test
    public void moreNumberFormats() {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(new Locale("en"));
        formatSymbols.setDecimalSeparator('.');
        formatSymbols.setGroupingSeparator(' ');
        NumberFormat formatSpaceDot = new DecimalFormat("#,##0.###", formatSymbols);
        
        final List<NumberFormat> formats= Arrays.asList(numberFormat, formatSpaceDot);

        assertTrue(NumberUtils.cleanBigDecimal("1 000.00", formats).equals(new BigDecimal(1000)));
        assertTrue(NumberUtils.cleanBigDecimal("1000 d", formats).equals(new BigDecimal(1000)));
        assertTrue(NumberUtils.cleanBigDecimal("10aa00", formats).equals(new BigDecimal(10)));
        assertTrue(NumberUtils.cleanBigDecimal("1,000", formats).equals(new BigDecimal(1000)));
        assertTrue(NumberUtils.cleanBigDecimal("1,000.500", formats).equals(new BigDecimal(1000.5)));
        assertTrue(NumberUtils.cleanBigDecimal("1 000.500", formats).equals(new BigDecimal(1000.5)));
        assertTrue(NumberUtils.cleanBigDecimal("1000.500", formats).equals(new BigDecimal(1000.5)));
        assertTrue(NumberUtils.cleanBigDecimal("1000 00", formats).equals(new BigDecimal(100000)));
    }
}
