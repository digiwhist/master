package eu.dl.utils;

import org.junit.Test;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertEquals;

/**
 * @author Tomas Mrazek
 */
public class FileUtilsTest {

    /**
     * Test of {@link eu.dl.utils.FileUtils#getMimeType(java.lang.String)}.
     */
    @Test
    public final void getMimeTypeTest() {
        assertNull(FileUtils.getMimeType(null));
        assertNull(FileUtils.getMimeType("file.abcd"));
        assertNull(FileUtils.getMimeType("file"));

        assertEquals("application/pdf", FileUtils.getMimeType(".pdf"));
        assertEquals("application/pdf", FileUtils.getMimeType("file.pdf"));
        // test of the case insensitivity
        assertEquals(FileUtils.getMimeType("file.pdf"), FileUtils.getMimeType("FILE.PDF"));
        assertEquals("application/pdf", FileUtils.getMimeType("path/to/the/file/file.pdf"));
        assertEquals("text/plain", FileUtils.getMimeType("file.txt"));
    }
}
