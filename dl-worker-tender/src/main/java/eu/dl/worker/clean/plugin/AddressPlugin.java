package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.AddressUtils;
import java.util.List;
import java.util.Map;

/**
 * Plugin used to clean tender addresses.
 *
 * @author Michal Riha
 */
public class AddressPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {

    private final Map<Enum, List<String>> countryMapping;

    /**
     * Default constructor. Plugin without country mapping.
     */
    public AddressPlugin() {
        this.countryMapping = null;
    }

    /**
     * Constructor with country mapping.
     *
     * @param countryMapping
     *          country ISO code mapping
     */
    public AddressPlugin(final Map<Enum, List<String>> countryMapping) {
        this.countryMapping = countryMapping;
    }
    

    /**
     * Cleans tender addresses.
     *
     * @param parsedTender
     *            tender with source data
     * @param cleanTender
     *            tender with clean data
     *
     * @return tender with cleaned tender addresses
     */
    @Override
    public final CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        return cleanTender
            .setAddressOfImplementation(AddressUtils.cleanAddress(parsedTender.getAddressOfImplementation(),
                countryMapping))
            .setDocumentsLocation(AddressUtils.cleanAddress(parsedTender.getDocumentsLocation(), countryMapping));
    }
}
