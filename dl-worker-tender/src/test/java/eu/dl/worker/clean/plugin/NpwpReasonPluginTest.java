package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.NpwpReason;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Test of clean method of NpwpReasonPlugin.
 */
public class NpwpReasonPluginTest {
    /**
     * Free text mapping test.
     */
    @Test
    public final void freeTextMappingCleaning() {
        Map<Enum, List<List<String>>> mapping = new HashMap<>();
        List<List<String>> map = new ArrayList<>();
        map.add(Arrays.asList("ADDITIONAL", "WORK"));
        map.add(Arrays.asList("add"));
        mapping.put(NpwpReason.ADDITIONAL_WORK, map);

        NpwpReasonPlugin plugin = new NpwpReasonPlugin(null, mapping);

        ParsedTender parsedTender = new ParsedTender()
            .addNpwpReason("ADDITIONAL_WORK").addNpwpReason("additional").addNpwpReason("abc");

        CleanTender cleanTender = plugin.clean(parsedTender, new CleanTender());

        // "ADDITIONAL_WORK" and "additional" strings will be matched
        assertEquals(2, cleanTender.getNpwpReasons().size());
        assertTrue(cleanTender.getNpwpReasons().stream().allMatch(n -> n == NpwpReason.ADDITIONAL_WORK));
    }

    /**
     * Exact match mapping test.
     */
    @Test
    public final void exactMatchMappingCleaning() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(NpwpReason.ADDITIONAL_WORK, Arrays.asList("ADDITIONAL", "WORK"));

        NpwpReasonPlugin plugin = new NpwpReasonPlugin(mapping);

        // only "additional" string will be matched
        ParsedTender parsedTender = new ParsedTender()
            .addNpwpReason("ADDITIONAL_WORK").addNpwpReason("additional").addNpwpReason("abc");

        CleanTender cleanTender = plugin.clean(parsedTender, new CleanTender());

        assertEquals(1, cleanTender.getNpwpReasons().size());
        assertTrue(cleanTender.getNpwpReasons().stream().allMatch(n -> n == NpwpReason.ADDITIONAL_WORK));
    }
}