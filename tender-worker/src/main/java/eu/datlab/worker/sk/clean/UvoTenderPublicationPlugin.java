package eu.datlab.worker.sk.clean;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BaseCleaningPlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * SK UVO Tender specific Publication plugin.
 */
public class UvoTenderPublicationPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {
    private final List<NumberFormat> numberFormat;
    private final List<DateTimeFormatter> formatters;
    private final Map<Enum, List<String>> formTypeMapping;

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat    number format
     * @param formatters      list of datetime formatters
     * @param formTypeMapping mapping for form type
     */
    public UvoTenderPublicationPlugin(final NumberFormat numberFormat, final List<DateTimeFormatter> formatters,
                                      final Map<Enum, List<String>> formTypeMapping) {
        this.formatters = formatters;
        this.numberFormat = Arrays.asList(numberFormat);
        this.formTypeMapping = formTypeMapping;
    }

    /**
     * Cleans URLs.
     *
     * @param parsedTender tender with source data
     * @param cleanTender  tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public final CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        return additionalClean(new PublicationPlugin(numberFormat, formatters, formTypeMapping)
                .clean(parsedTender, cleanTender));
    }

    /**
     * Additional SK UVO cleaning. Check if url is correction, if so, modify url and set form type to modification.
     *
     * @param cleanTender cleanTender to be additionally cleaned
     *
     * @return cleanTender
     */
    private CleanTender additionalClean(final CleanTender cleanTender) {
        final String correction = "?v=correction";

        if (cleanTender.getPublications() != null && !cleanTender.getPublications().isEmpty()) {
            // new list of publications that we will set to clean tender
            List<Publication> updatedPublications = new ArrayList<>();

            try {
                for (Publication publication : cleanTender.getPublications()) {
                    final String humanReadableUrl =
                        publication.getHumanReadableUrl() == null ? null : publication.getHumanReadableUrl().toString();

                    // if publication ends with correction like
                    // https://www.uvo.gov.sk/vestnik/oznamenie/detail/336106?v=correction
                    // create new publication for original form that is being updated. In this case
                    // https://www.uvo.gov.sk/vestnik/oznamenie/detail/336106
                    if (humanReadableUrl != null && humanReadableUrl.endsWith(correction)) {
                        // also set form type of correction form to CONTRACT_UPDATE
                        publication.setFormType(PublicationFormType.CONTRACT_UPDATE);

                        updatedPublications.add(new Publication()
                            .setSource(publication.getSource())
                            .setIsIncluded(false)
                            .setHumanReadableUrl(new URL(humanReadableUrl.replace(correction, ""))));
                    }

                    final String machineReadableUrl =
                        publication.getMachineReadableUrl() == null ? null : publication.getMachineReadableUrl().toString();

                    if (machineReadableUrl != null && machineReadableUrl.endsWith(correction)) {
                        publication.setFormType(PublicationFormType.CONTRACT_UPDATE);

                        updatedPublications.add(new Publication()
                            .setSource(publication.getSource())
                            .setIsIncluded(false)
                            .setHumanReadableUrl(new URL(machineReadableUrl.replace(correction, ""))));
                    }

                    updatedPublications.add(publication);
                }
            } catch (MalformedURLException e) {
                logger.error("Correction Publication URL cleaning failed with {}", e);
                throw new UnrecoverableException("Correction Publication URL cleaning failed with ", e);
            }

            cleanTender.setPublications(updatedPublications);
        }

        return cleanTender;
    }
}
