package eu.dl.worker.clean.utils.date;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import eu.dl.worker.clean.utils.DateUtils;

/**
 * Test of basic date time cleaning.
 *
 * @author Tomas Mrazek
 */
public final class CleanDateTimeTest {
    private final List<DateTimeFormatter> formatters = Arrays.asList(
        DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss"), DateTimeFormatter.ofPattern("uuuuMMdd HH:mm:ss"));

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
        assertTrue(LocalDateTime.parse("2012-12-03 12:00:00", formatters.get(0))
            .equals(DateUtils.cleanDateTime("2012-12-03 12:00:00", formatters.get(0))));

        assertTrue(LocalDateTime.parse("2012-12-03 12:00:00", formatters.get(0))
            .equals(DateUtils.cleanDateTime("2012-12-03 12:00:00", formatters)));

        assertTrue(LocalDateTime.parse("20121203 12:00:00", formatters.get(1))
            .equals(DateUtils.cleanDateTime("20121203 12:00:00", formatters)));

        assertTrue(DateUtils.cleanDateTime("2012-12-03 12:00:00", formatters)
            .equals(DateUtils.cleanDateTime("20121203 12:00:00", formatters)));

        // will autocorrect the wrong date in Feb
        assertTrue(LocalDateTime.parse("2012-02-29 12:00:00", formatters.get(0))
            .equals(DateUtils.cleanDateTime("2012-02-30 12:00:00", formatters)));
    }

    /**
     * Malformed values - should throw an exception.
     */
    @Test
    public void malformedTest() {
        assertNull(DateUtils.cleanDateTime("orange", formatters.get(0)));
        assertNull(DateUtils.cleanDateTime("2012-12-03", formatters.get(0)));
        assertNull(DateUtils.cleanDateTime("2012-03-30 28:00:00", formatters.get(0)));
    }

    /**
     * Nonsensical values test.
     */
    @Test
    public void nonsensicalValuesTest() {
        assertNull(DateUtils.cleanDateTime("1999-12-31 12:00:00", formatters.get(0)));
        assertNull(DateUtils.cleanDateTime("2025-01-02 12:00:00", formatters.get(0)));
    }
}
