package eu.dl.worker.clean.plugin;

import java.net.URL;
import java.util.List;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.StringUtils;
import eu.dl.worker.clean.utils.URLSchemeType;
import eu.dl.worker.utils.ArrayUtils;

/**
 * Plugin used to clean URLs.
 *
 * @author Tomas Mrazek
 */
public class URLPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {

    /**
     * Cleans URLs.
     *
     * @param parsedTender
     *            tender with source data
     * @param cleanTender
     *            tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public final CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        if (parsedTender.getCourtInterventions() != null) {
            final List<URL> cleanCourtInterventions = ArrayUtils.walk(parsedTender.getCourtInterventions(),
                (parsedURL) -> StringUtils.cleanURL(parsedURL, URLSchemeType.HTTP));

            cleanTender.setCourtInterventions(cleanCourtInterventions);

            logger.debug("Cleaned courtInterventions in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                cleanCourtInterventions);
        }

        if (parsedTender.getCourtProceedings() != null) {
            final List<URL> cleanCourtProceedings = ArrayUtils.walk(parsedTender.getCourtProceedings(),
                (parsedURL) -> StringUtils.cleanURL(parsedURL, URLSchemeType.HTTP));

            cleanTender.setCourtProceedings(cleanCourtProceedings);

            logger.debug("Cleaned courtProceedings in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                cleanCourtProceedings);
        }

        return cleanTender;
    }
}
