package eu.dl.worker.utils.textFactory;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Plain text factory.
 */
public final class PlainTextService {

    private static final Logger logger = LoggerFactory.getLogger(PlainTextService.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private PlainTextService() {
    }

    /**
     * Parse text from doc, xls, ppt, rtf, pdf, html, xhtml, OpenDocument, txt, vsd.
     *
     * @param inputStream input stream of file to be parsed from
     * @return String or null
     */
    public static String parseTextFrom(final InputStream inputStream) {
        if (inputStream != null) {
            try {
                final Tika parser = new Tika();
                // disable max string length limit
                parser.setMaxStringLength(-1);
                final String result = parser.parseToString(inputStream);

                // String \n400 is causing problems with Postgres, thus removing
                return result == null ? null : result.replaceAll("\\n400", "");
            } catch (IOException | TikaException | NoClassDefFoundError e) {
                logger.warn("Parsing text from file failed with (corrupted file possibly):", e);
            }
        }

        return null;
    }

    /**
     * Reads all text from a text file.
     *
     * @param path
     *      text file path
     * @param encoding
     *      encoding of characters
     *
     * @return String or null
     * @throws IOException
     *         if an I/O error occurs reading from the stream
     */
    public static String readFile(final String path, final Charset encoding) throws IOException {
        byte[] encoded = Files.readAllBytes(Paths.get(path));
        return new String(encoded, encoding);
    }

}
