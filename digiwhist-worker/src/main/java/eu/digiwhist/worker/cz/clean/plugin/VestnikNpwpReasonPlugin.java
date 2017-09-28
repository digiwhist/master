package eu.digiwhist.worker.cz.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.CodeTablePlugin;
import eu.dl.worker.clean.plugin.NpwpReasonPlugin;
import org.apache.commons.lang.BooleanUtils;

import java.util.List;
import java.util.Map;

/**
 * Plugin used to clean NPWP reason.
 */
public final class VestnikNpwpReasonPlugin extends CodeTablePlugin<ParsedTender, CleanTender> {

    /**
     * NPWP reason cleaning plugin with mapping.
     *
     * @param mapping
     *         mapping of the values
     */
    public VestnikNpwpReasonPlugin(final Map<Enum, List<String>> mapping) {
        super(mapping);
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
        // Czech VVZ forms have textual npwp reason which cannot be mapped to any value
        if (isCzechVVZForm(parsedTender)) {
            logger.debug("Cleaning NpwpReasons for Czech VVZ forms is skipped for tender {}.", parsedTender.getId());
            return cleanTender;
        }
        return new NpwpReasonPlugin(mapping).clean(parsedTender, cleanTender);
    }

    /**
     * Checks whether the form is Czech VVZ form (that is any CZ* form).
     *
     * @param parsedTender
     *         parsed tender
     *
     * @return true if the tender is parsed from Czech VVZ form
     */
    private static boolean isCzechVVZForm(final ParsedTender parsedTender) {
        return parsedTender.getPublications().stream().anyMatch(VestnikNpwpReasonPlugin::isCzechVVZPublication);
    }

    /**
     * Checks whether the publication is primary (included) and is a publication of Czech national form.
     *
     * @param publication
     *         parsed publication
     *
     * @return true if the publication is primary (included) and is a publication of Czech national form
     */
    private static boolean isCzechVVZPublication(final ParsedPublication publication) {
        return publication != null && BooleanUtils.toBoolean(
                publication.getIsIncluded()) && publication.getSourceFormType().startsWith("CZ");
    }
}
