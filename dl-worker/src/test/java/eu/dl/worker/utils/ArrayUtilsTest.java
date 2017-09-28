package eu.dl.worker.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.text.NumberFormat;
import java.util.Arrays;
import java.util.Locale;

import org.junit.Test;

import eu.dl.worker.clean.utils.NumberUtils;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Tomas Mrazek
 */
public final class ArrayUtilsTest {
    private final NumberFormat numberFormat = NumberFormat.getInstance(new Locale("en_US"));

    /**
     * Test of {@link ArrayUtils#walk(java.util.List, java.util.function.Function)}.
     */
    @Test
    public void arrayWalkTest() {
        // null values
        assertNull(ArrayUtils.walk(Arrays.asList(null, ""), (item) -> NumberUtils.cleanInteger(item, numberFormat)));

        // ok values
        assertEquals(
            Arrays.asList(1, 2),
            ArrayUtils.walk(Arrays.asList("1 ", "2"), (item) -> NumberUtils.cleanInteger(item, numberFormat)));

        assertEquals(
            Arrays.asList(1, 2),
            ArrayUtils.walk(Arrays.asList("1 ", "2", null, ""),
                (item) -> NumberUtils.cleanInteger(item, numberFormat)));
    }

    /**
     * Test of {@link ArrayUtils#distinct(java.util.function.Function)}.
     */
    @Test
    public void distinctTest() {
        Map<String, Integer> map = new HashMap<>();
        map.put("a", 1);
        map.put("b", 2);
        map.put("c", 1);
        map.put("d", 1);

        // defined key extractor
        assertEquals(2, map.entrySet().stream()
            .filter(ArrayUtils.distinct(n -> n.getValue()))
            .collect(Collectors.toSet()).size());


        // uses Objects#toString on objects, no data reduction
        assertEquals(4, map.entrySet().stream()
            .filter(ArrayUtils.distinct())
            .collect(Collectors.toSet()).size());

        // uses Objects#toString on primitive types, data reduction
        assertEquals(2, Arrays.asList("a", "b", "a", "a", "b").stream()
            .filter(ArrayUtils.distinct())
            .collect(Collectors.toSet()).size());

        assertEquals(3, Arrays.asList(1, 1, 2, 3, 2).stream()
            .filter(ArrayUtils.distinct())
            .collect(Collectors.toSet()).size());
    }

    /**
     * Test for {@link ArrayUtils#max(java.util.Comparator)}.
     */
    @Test
    public void maxTest() {
        // longest strings are collected
        List<String> strings = Arrays.asList("a", "aa", "bb", "c").stream()
            .collect(ArrayUtils.max((a, b) -> Integer.compare(a.length(), b.length())));
        
        assertEquals(2, strings.size());
        assertEquals("aa", strings.get(0));
        assertEquals("bb", strings.get(1));


        // entries with highest value are collected
        Map<String, Float> map = new HashMap<>();
        map.put("a", 1f);
        map.put("b", 2f);
        map.put("c", 2f);

       List<Map.Entry<String, Float>> entries = map.entrySet().stream()
           .collect(ArrayUtils.max((a, b) -> Float.compare(a.getValue(), b.getValue())));

        assertEquals(2, entries.size());
        assertEquals(2f, entries.get(0).getValue(), 0);
        assertEquals(2f, entries.get(1).getValue(), 0);
    }
}
