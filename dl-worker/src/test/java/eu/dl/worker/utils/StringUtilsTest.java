package eu.dl.worker.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;

import org.junit.Test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.nio.charset.StandardCharsets.ISO_8859_1;

/**
 * StringUtils test class.
 *
 * @author Tomas Mrazek
 */
public final class StringUtilsTest {

    private static final String TEST_STRING = "header\nline 1\nline 2\nline 3\nline 4";

    private static final Charset WINDOWS_1250 = Charset.forName("Windows-1250");

    /**
     * Test for method {@link StringUtils#chunkStringByLines(java.lang.String, int, int)}.
     *
     * @throws IOException
     *      If an I/O error occurs
     */
    @Test
    public void chunkStringByLinesTest() throws IOException {
        List<String> chunks = StringUtils.chunkStringByLines(TEST_STRING, 2, 1);
        assertEquals(chunks.size(), 2);
        assertEquals(chunks.get(0), "line 1\nline 2");
        assertEquals(chunks.get(1), "line 3\nline 4");

        List<String> oneChunk = StringUtils.chunkStringByLines(TEST_STRING, 10, 0);
        assertEquals(oneChunk.size(), 1);
        assertEquals(oneChunk.get(0), TEST_STRING);
        

        assertEquals(StringUtils.chunkStringByLines("", 2, 1).size(), 0);
        assertEquals(StringUtils.chunkStringByLines(TEST_STRING, 0, 1).size(), 0);
        assertEquals(StringUtils.chunkStringByLines(null, 0, 1).size(), 0);
    }

    /**
     * Test for method {@link StringUtils#chunkStringByLines(java.lang.String, int, int, java.util.function.Predicate).
     *
     * @throws IOException
     *      If an I/O error occurs
     */
    @Test
    public void chunkStringByLinesWithFilterTest() throws IOException {
        List<String> chunks = StringUtils.chunkStringByLines(TEST_STRING, 2, 1, l -> l.matches("line (2|4)"));
        assertEquals(chunks.size(), 1);
        assertEquals(chunks.get(0), "line 2\nline 4");
    }

    /**
     * Test for method {@link StringUtils#head(java.lang.String, int)}.
     */
    @Test
    public void headTest() {
        assertEquals(StringUtils.head(TEST_STRING, 1), "header");
        assertEquals(StringUtils.head(TEST_STRING, 10), TEST_STRING);


        assertNull(StringUtils.head(null, 1));
        assertNull(StringUtils.head(TEST_STRING, 0));
        assertNull(StringUtils.head(TEST_STRING, -1));
    }

    /**
     * Test for method {@link StringUtils#tail(java.lang.String, int)}.
     */
    @Test
    public void tailTest() {
        assertEquals(StringUtils.tail(TEST_STRING, 1), "line 4");
        assertEquals(StringUtils.tail(TEST_STRING, 10), TEST_STRING);

        
        assertNull(StringUtils.tail(null, 1));
        assertNull(StringUtils.tail(TEST_STRING, 0));
        assertNull(StringUtils.tail(TEST_STRING, -1));
    }

    /**
     * Test for method {@link StringUtils#justifyLeft(java.lang.String, int, java.lang.String)}.
     */
    @Test
    public void justifyLeftTest() {
        assertNull(StringUtils.justifyLeft(null, 1, "0"));
        assertEquals("2345", StringUtils.justifyLeft("12345", 4, "0"));
        assertEquals("1234", StringUtils.justifyLeft("1234", 4, "0"));
        assertEquals("0012", StringUtils.justifyLeft("12", 4, "0"));
    }

    /**
     * Test for methods {@link StringUtils#charset(String, Charset...)} and {@link StringUtils#convert(String, Charset, Charset)}.
     */
    @Test
    public void charsetAndConvertTest() {
        Charset[] charsets = new Charset[]{ISO_8859_1, WINDOWS_1250};
        String utf8 = "ěščřžýáíéúů";
        String iso88591 = "Ä\u009BÅ¡Ä\u008DÅ\u0099Å¾Ã½Ã¡Ã\u00ADÃ©ÃºÅ¯";
        String windows1250 = "Ä›ĹˇÄŤĹ™ĹľĂ˝ĂˇĂ\u00ADĂ©ĂşĹŻ";

        assertNull(StringUtils.charset(null, charsets));
        assertNull(StringUtils.charset(utf8, ISO_8859_1));
        assertNull(StringUtils.charset(utf8, ISO_8859_1, null));
        assertNull(StringUtils.convert(null, ISO_8859_1, UTF_8));

        assertEquals(utf8, StringUtils.convert(utf8, null, UTF_8));
        assertEquals(utf8, StringUtils.convert(utf8, ISO_8859_1, null));

        assertEquals(ISO_8859_1, StringUtils.charset(iso88591, charsets));
        assertEquals(utf8, StringUtils.convert(iso88591, ISO_8859_1, UTF_8));
        assertEquals(utf8, StringUtils.convert(iso88591, StringUtils.charset(iso88591, charsets), UTF_8));

        assertEquals(WINDOWS_1250, StringUtils.charset(windows1250, charsets));
        assertEquals(utf8, StringUtils.convert(windows1250, WINDOWS_1250, UTF_8));
        assertEquals(utf8, StringUtils.convert(windows1250, StringUtils.charset(windows1250, charsets), UTF_8));

        assertEquals(null, StringUtils.charset(utf8, charsets));
        assertEquals(utf8, StringUtils.convert(utf8, StringUtils.charset(utf8, charsets), UTF_8));
    }
}
