package eu.datlab.worker.py.parsed;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.MissingNode;
import com.fasterxml.jackson.databind.node.NumericNode;
import com.fasterxml.jackson.databind.node.TextNode;
import java.io.IOException;
import org.junit.Test;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;

/**
 *
 * @author Tomas Mrazek
 */
public class DNCPParserUtilsTest {

    private static final String JSON = "{"
        + "\"string\":\"hello world\","
        + "\"array\": [1, 2, 3],"
        + "\"nested\": {\"path\":\"nested\"}"
        + "}";

    /**
     * Test of {@link eu.datlab.worker.py.parsed.DNCPParserUtils#path(java.lang.String, com.fasterxml.jackson.databind.JsonNode).
     *
     * @throws IOException in case that it is unable to parse JSON. It shouldn't happen.
     */
    @Test
    public final void pathTest() throws IOException {
        ObjectMapper mapper = new ObjectMapper();
        JsonNode json = mapper.readTree(JSON);

        assertTrue(DNCPParserUtils.path(null, json) instanceof MissingNode);
        assertTrue(DNCPParserUtils.path("string", null) instanceof MissingNode);
        assertTrue(DNCPParserUtils.path("unknown/path", json) instanceof MissingNode);

        assertTrue(DNCPParserUtils.path("string", json) instanceof TextNode);
        assertTrue(DNCPParserUtils.path("array", json) instanceof ArrayNode);
        assertTrue(DNCPParserUtils.path("array/0", json) instanceof NumericNode);
        assertTrue(DNCPParserUtils.path("nested/path", json) instanceof TextNode);        
    }

    /**
     * Test of {@link eu.datlab.worker.py.parsed.DNCPParserUtils#textValue(java.lang.String, com.fasterxml.jackson.databind.JsonNode).
     *
     * @throws IOException in case that it is unable to parse JSON. It shouldn't happen.
     */
    @Test
    public final void textValueTest() throws IOException {
        ObjectMapper mapper = new ObjectMapper();
        JsonNode json = mapper.readTree(JSON);

        assertEquals(null, DNCPParserUtils.textValue(null, json));
        assertEquals(null, DNCPParserUtils.textValue("string", null));
        assertEquals(null, DNCPParserUtils.textValue("unknown/path", json));

        assertEquals("hello world", DNCPParserUtils.textValue("string", json));
        assertEquals("1", DNCPParserUtils.textValue("array/0", json));
        assertEquals("nested", DNCPParserUtils.textValue("nested/path", json));
    }
}
