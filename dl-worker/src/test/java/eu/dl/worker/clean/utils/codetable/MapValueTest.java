package eu.dl.worker.clean.utils.codetable;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import eu.dl.worker.clean.utils.CodeTableUtils;
import java.util.Collections;

/**
 * Test of mapping between string value and codetable.
 * 
 * @author Kuba Krafka
 */
public final class MapValueTest {

    private Map<Enum, List<String>> mapping;

    /**
     * Initialisation of mapping used in in test methods.
     */
    @Before
    public void init() {
        mapping = new HashMap<Enum, List<String>>();
        mapping.put(Fruits.APPLE, Arrays.asList("apples", "aple", "aplpe", "apple", "lev123"));
        mapping.put(Fruits.MAPPLE, Arrays.asList("mapples", "ample", "mapple", "lev123"));
    }

    /**
     * Test of null value.
     */
    @Test
    public void nullValueTest() {
        assertNull(CodeTableUtils.mapValue(null, null));
    }

    /**
     * Test value search with a simple match(equality).
     */
    @Test
    public void simpleMatchTestValues() {
        assertTrue(Fruits.APPLE.equals(CodeTableUtils.mapValue("apple", mapping)));
        assertTrue(Fruits.APPLE.equals(CodeTableUtils.mapValue("aple", mapping)));
        assertTrue(Fruits.MAPPLE.equals(CodeTableUtils.mapValue("mapple", mapping)));
    }
    
    /**
     * Test value search with a simple match(equality).
     */
    @Test
    public void levenshteinMatchTestValues() {
        assertTrue(Fruits.APPLE.equals(CodeTableUtils.mapValue("apple12t", mapping)));
        assertTrue(Fruits.MAPPLE.equals(CodeTableUtils.mapValue("sample", mapping)));
        // both keys should have the same distance, unable to decide
        assertNull(CodeTableUtils.mapValue("lev123a", mapping));
        assertNull(CodeTableUtils.mapValue("lev123aa", mapping));
        assertNull(CodeTableUtils.mapValue("lev123aaa", mapping));
        // distance higher the threshold
        assertNull(CodeTableUtils.mapValue("lev123aaaa", mapping));
    }

    /**
     * Not found values - should throw an exception.
     */
    @Test
    public void notFoundTest() {
        assertNull(CodeTableUtils.mapValue("orange", mapping));

    }

    /**
     * Test value search with default value.
     */
    @Test
    public void defaultMatchTestValues() {
        assertEquals(Fruits.APPLE, CodeTableUtils.mapValue("apple", mapping, Fruits.APPLE));
        assertEquals(Fruits.APPLE, CodeTableUtils.mapValue("orange", mapping, Fruits.APPLE));
        assertEquals(Fruits.APPLE, CodeTableUtils.mapValue("orange", Collections.emptyMap(), Fruits.APPLE));
        assertNull(CodeTableUtils.mapValue("apple", null, Fruits.APPLE));
    }

}
