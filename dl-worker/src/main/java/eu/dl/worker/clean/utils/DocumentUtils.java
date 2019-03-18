package eu.dl.worker.clean.utils;

import eu.dl.worker.utils.ArrayUtils;
import eu.dl.dataaccess.dto.codetables.DocumentType;
import eu.dl.dataaccess.dto.generic.Document;
import eu.dl.dataaccess.dto.parsed.ParsedDocument;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * This class provide method for address cleaning.
 *
 * @author Tomas Mrazek
 */
public final class DocumentUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private DocumentUtils() {

    }

    /**
     * Cleans the given parsed document.
     *
     * @param parsedDocument
     *          parsed document
     * @param numberFormats
     *          list of number formats
     * @param formatters
     *          date and datetime formatters
     * @param typeMapping
     *          document type mapping
     * @return clean document
     */
    public static Document cleanDocument(final ParsedDocument parsedDocument,
        final List<NumberFormat> numberFormats, final List<DateTimeFormatter> formatters,
        final Map<Enum, List<String>> typeMapping) {
        if (parsedDocument == null) {
            return null;
        }

        return new Document()
            .setDescription(StringUtils.cleanLongString(parsedDocument.getDescription()))
            .setExtensions(ArrayUtils.walk(parsedDocument.getExtensions(),
                (parsedExtension) -> DocumentUtils.cleanDocument(parsedExtension, numberFormats, formatters,
                    typeMapping)))
            .setFormat(StringUtils.cleanShortString(parsedDocument.getFormat()))
            .setLanguage(StringUtils.cleanShortString(parsedDocument.getLanguage()))
            .setOrder(NumberUtils.cleanInteger(parsedDocument.getOrder(), numberFormats))
            .setOtherVersions(ArrayUtils.walk(parsedDocument.getOtherVersions(),
                (parsedVersion) -> DocumentUtils.cleanDocument(parsedVersion, numberFormats, formatters, typeMapping)))
            .setPublicationDateTime(DateUtils.cleanDateTime(parsedDocument.getPublicationDateTime(), formatters))
            .setSignatureDate(DateUtils.cleanDate(parsedDocument.getSignatureDate(), formatters))
            .setTitle(StringUtils.cleanShortString(parsedDocument.getTitle()))
            .setType((DocumentType) CodeTableUtils.mapValue(parsedDocument.getType(), typeMapping))
            .setUrl(StringUtils.cleanURL(parsedDocument.getUrl(), URLSchemeType.HTTP))
            .setPlainDocumentId(parsedDocument.getPlainDocumentId())
            .setVersion(StringUtils.cleanShortString(parsedDocument.getVersion()));
    }

    /**
     * Cleans the given parsed document.
     *
     * @param parsedDocument
     *          parsed document
     * @param numberFormat
     *          number format
     * @param formatters
     *          date and datetime formatters
     * @param typeMapping
     *          document type mapping
     * @return clean document
     */
    public static Document cleanDocument(final ParsedDocument parsedDocument, final NumberFormat numberFormat,
            final List<DateTimeFormatter> formatters, final Map<Enum, List<String>> typeMapping) {
        return cleanDocument(parsedDocument, Arrays.asList(numberFormat), formatters, typeMapping);
    }
}
