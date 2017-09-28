package eu.dl.worker.clean.utils;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;


/**
 * This class provide method for publication cleaning.
 *
 * @author Tomas Mrazek
 */
public final class PublicationUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private PublicationUtils() {

    }

    /**
     * Cleans the given publication.
     *
     * @param parsedPublication
     *          parsed publication
     * @param numberFormats
     *          list of number formats
     * @param formatter
     *          datetime formatter
     * @param formTypeMapping
     *          form type mapping
     * @return clean publication
     */
    public static Publication cleanPublication(
        final ParsedPublication parsedPublication,
        final List<NumberFormat> numberFormats,
        final List<DateTimeFormatter> formatter,
        final Map<Enum, List<String>> formTypeMapping) {
        if (parsedPublication == null) {
            return null;
        }

        return new Publication()
            .setBuyerAssignedId(StringUtils.cleanShortString(parsedPublication.getBuyerAssignedId()))
            .setDispatchDate(DateUtils.cleanDate(parsedPublication.getDispatchDate(), formatter))
            //form type is determined on the base of sourceFormType value
            .setFormType((PublicationFormType) CodeTableUtils.mapValue(
                    StringUtils.cleanShortString(parsedPublication.getSourceFormType()), formTypeMapping,
                    PublicationFormType.OTHER, false))
            .setHumanReadableUrl(
                    StringUtils.cleanURL(parsedPublication.getHumanReadableUrl(), URLSchemeType.HTTP))
            .setIsIncluded(StringUtils.cleanBoolean(parsedPublication.getIsIncluded()))
            .setIsParentTender(StringUtils.cleanBoolean(parsedPublication.getIsParentTender()))
            .setIsValid(StringUtils.cleanBoolean(parsedPublication.getIsValid()))
            .setLanguage(StringUtils.cleanShortString(parsedPublication.getLanguage()))
            .setLastUpdate(DateUtils.cleanDate(parsedPublication.getLastUpdate(), formatter))
            .setMachineReadableUrl(
                    StringUtils.cleanURL(parsedPublication.getMachineReadableUrl(), URLSchemeType.HTTP))
            .setPublicationDate(
                    DateUtils.cleanDate(parsedPublication.getPublicationDate(), formatter))
            .setSource(StringUtils.cleanURL(parsedPublication.getSource(), URLSchemeType.HTTP))
            .setSourceFormType(StringUtils.cleanShortString(parsedPublication.getSourceFormType()))
            .setSourceId(StringUtils.cleanShortString(parsedPublication.getSourceId()))
            .setSourceTenderId(StringUtils.cleanShortString(parsedPublication.getSourceTenderId()))
            .setVersion(NumberUtils.cleanInteger(parsedPublication.getVersion(), numberFormats));
    }

    /**
     * Cleans the given publication.
     *
     * @param parsedPublication
     *          parsed publication
     * @param numberFormat
     *          number format
     * @param formatter
     *          datetime formatter
     * @param formTypeMapping
     *          form type mapping
     * @return clean publication
     */
    public static Publication cleanPublication(final ParsedPublication parsedPublication,
        final NumberFormat numberFormat, final List<DateTimeFormatter> formatter,
        final Map<Enum, List<String>> formTypeMapping) {
        return cleanPublication(parsedPublication, Arrays.asList(numberFormat), formatter, formTypeMapping);
    }
}
