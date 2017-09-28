package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.DateUtils;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;

/**
 * Plugin used to clean date fields.
 *
 * @author Kuba Krafka
 *
 * @param <T>
 *      parsable item
 * @param <U>
 *      cleanable item
 */
public final class DatePlugin<T extends ParsedTender, U extends CleanTender>
    extends BaseDateTimePlugin<DatePlugin, T, U> {

    /**
     * DatePlugin should be initialised with the pattern of the date.
     *
     * @param formatter
     *            formatter used to parse the date/datetime fields
     */
    public DatePlugin(final DateTimeFormatter formatter) {
        super(formatter);
    }

    /**
     * Plugin constructor with configuration.
     *
     * @param formatters
     *       list of datetime formatters
     */
    public DatePlugin(final List<DateTimeFormatter> formatters) {
        super(formatters);
    }

    /**
     * Cleans date and date fields.
     *
     * @param parsedTender
     *            tender with source data
     * @param cleanTender
     *            tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public U clean(final T parsedTender, final U cleanTender) {
        final LocalDate awardDeadline = DateUtils.cleanDate(parsedTender.getAwardDeadline(), formatters);
        cleanTender.setAwardDeadline(awardDeadline);
        logger.debug("Cleaned awardDeadline for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            awardDeadline);

        final LocalDate awardDecisionDate = DateUtils.cleanDate(parsedTender.getAwardDecisionDate(), formatters);
        cleanTender.setAwardDecisionDate(awardDecisionDate);
        logger.debug("Cleaned awardDecisionDate for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            awardDecisionDate);

        final LocalDate cancellationDate = DateUtils.cleanDate(parsedTender.getCancellationDate(), formatters);
        cleanTender.setCancellationDate(cancellationDate);
        logger.debug("Cleaned cancellationDate for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            cancellationDate);

        final LocalDate contractSignatureDate =
            DateUtils.cleanDate(parsedTender.getContractSignatureDate(), formatters);
        cleanTender.setContractSignatureDate(contractSignatureDate);
        logger.debug("Cleaned contractSignatureDate for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            contractSignatureDate);

        final LocalDate enquiryDeadline = DateUtils.cleanDate(parsedTender.getEnquiryDeadline(), formatters);
        cleanTender.setEnquiryDeadline(enquiryDeadline);
        logger.debug(
            "Cleaned enquiryDeadline for parsed tender {}, clean value \"{}\"", parsedTender.getId(), enquiryDeadline);

        final LocalDate estimatedCompletionDate =
            DateUtils.cleanDate(parsedTender.getEstimatedCompletionDate(), formatters);
        cleanTender.setEstimatedCompletionDate(estimatedCompletionDate);
        logger.debug("Cleaned estimatedCompletionDate for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            estimatedCompletionDate);

        final LocalDate estimatedStartDate = DateUtils.cleanDate(parsedTender.getEstimatedStartDate(), formatters);
        cleanTender.setEstimatedStartDate(estimatedStartDate);
        logger.debug("Cleaned estimatedStartDate for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            estimatedStartDate);

        return cleanTender;
    }

}
