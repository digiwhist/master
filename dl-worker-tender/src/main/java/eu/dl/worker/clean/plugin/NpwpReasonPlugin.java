package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.NpwpReason;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.ArrayUtils;
import eu.dl.worker.clean.utils.CodeTableUtils;
import java.util.ArrayList;

import java.util.List;
import java.util.Map;

/**
 * Plugin for cleaning negotiated procedure without publication reason.
 */
public final class NpwpReasonPlugin extends CodeTablePlugin<ParsedTender, CleanTender> {

    /**
     * NPWP reason cleaning plugin with mapping.
     *
     * @param mapping
     *         mapping of the values
     */
    public NpwpReasonPlugin(final Map<Enum, List<String>> mapping) {
        super(mapping);
    }

    /**
     * NPWP reason cleaning plugin with mapping.
     *
     * @param mapping
     *         mapping of the values
     * @param freeTextMapping
     *         mapping of the values
     */
    public NpwpReasonPlugin(final Map<Enum, List<String>> mapping, final Map<Enum, List<List<String>>> freeTextMapping) {
        super(mapping, freeTextMapping);
    }

    /**
     * Cleans npwpReasons field.
     *
     * @param parsedTender
     *         tender with source data
     * @param cleanTender
     *         tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        logger.debug("Cleaning NpwpReasons in parsed tender {} starts", parsedTender.getId());
        List<NpwpReason> cleanedNpwpReasons = ArrayUtils.walk(parsedTender.getNpwpReasons(),
                npwpReason -> (NpwpReason) CodeTableUtils.mapValue(npwpReason, mapping));
        
        if (cleanedNpwpReasons == null) {
            cleanedNpwpReasons = new ArrayList<>();
        }

        if (this.freeTextMapping != null && !this.freeTextMapping.isEmpty()) {
            if (parsedTender.getNpwpReasons() != null && !parsedTender.getNpwpReasons().isEmpty()) {
                for (String npwpReason : parsedTender.getNpwpReasons()) {
                    for (Map.Entry<Enum, List<List<String>>> entry : freeTextMapping.entrySet()) {
                        NpwpReason key = (NpwpReason) entry.getKey();

                        List<List<String>> mappings = entry.getValue();

                        for (List<String> map : mappings) {
                            Boolean allIn = true;
                            for (String needle : map) {
                                if (!npwpReason.toLowerCase().contains(needle.toLowerCase())) {
                                    allIn = false;
                                    break;
                                }
                            }

                            if (allIn) {
                                cleanedNpwpReasons.add(key);
                                break;
                            }
                        }
                    }
                }
            }
        }

        if (!cleanedNpwpReasons.isEmpty()) {
            cleanTender.setNpwpReasons(cleanedNpwpReasons);
        }

        logger.debug("Cleaning NpwpReasons in parsed tender {} finished", parsedTender.getId());
        return cleanTender;
    }
}
