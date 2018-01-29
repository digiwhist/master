package eu.dl.worker.clean.utils.date;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import eu.dl.worker.clean.utils.DateUtils;

/**
 * Test of basic date cleaning.
 *
 * @author Kuba Krafka
 */
public final class CleanDateTest {
    private final List<DateTimeFormatter> formatters =
        Arrays.asList(DateTimeFormatter.ofPattern("uuuu-MM-dd"), DateTimeFormatter.ofPattern("uuuuMMdd"));

    /**
     * Test of null value.
     */
    @Test
    public void nullValueTest() {
        assertNull(DateUtils.cleanDate(null, formatters.get(0)));
        assertNull(DateUtils.cleanDate(null, formatters));
    }

    /**
     * Test of successful date cleaning.
     */
    @Test
    public void simpleMatchTestValues() {
        assertTrue(LocalDate.parse("2012-12-03", formatters.get(0))
            .equals(DateUtils.cleanDate("2012-12-03", formatters.get(0))));

        assertTrue(LocalDate.parse("2012-12-03", formatters.get(0))
            .equals(DateUtils.cleanDate("2012-12-03", formatters)));

        assertTrue(LocalDate.parse("20121203", formatters.get(1))
            .equals(DateUtils.cleanDate("20121203", formatters)));

        assertTrue(DateUtils.cleanDate("2012-12-03", formatters)
            .equals(DateUtils.cleanDate("20121203", formatters)));

        // will autocorrect the wrong date in Feb
        assertTrue(LocalDate.parse("2012-02-29", formatters.get(0))
            .equals(DateUtils.cleanDate("2012-02-30", formatters)));
    }

    /**
     * Malformed values - should throw an exception.
     */
    @Test
    public void malformedTest() {
        assertNull(DateUtils.cleanDate("orange", formatters.get(0)));
        assertNull(DateUtils.cleanDate("2012-03-32", formatters.get(0)));
    }

    /**
     * Nonsensical values test.
     */
    @Test
    public void nonsensicalValuesTest() {
        assertNull(DateUtils.cleanDate("1999-12-31", formatters.get(0)));
        assertNull(DateUtils.cleanDate("2025-01-02", formatters.get(0)));
    }
}
