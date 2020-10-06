package eu.dl.worker.utils;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.apache.commons.codec.digest.DigestUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Iterator;
import java.util.List;

/**
 * Provides checksum.
 */
public final class ChecksumUtils {
    private static final Logger logger = LoggerFactory.getLogger(ChecksumUtils.class);

    private static final ObjectMapper mapper;
    static {
        mapper = new ObjectMapper();
        mapper.enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS);
        mapper.registerModule(new JavaTimeModule());
        mapper.setDateFormat(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"));
        mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }

    /**
     * Suppress default constructor.
     */
    private ChecksumUtils() {
    }

    /**
     * @param <T>
     *      class of input object
     * @param t
     *      parsed tender to be cloned
     * @return clone of the given parsed tender
     *
     * @throws UnrecoverableException if the serialization or deserialization fails.
     */
    public static <T> T clone(final T t) {
        try {
            String jsonSource = mapper.writeValueAsString(t);
            return mapper.readValue(jsonSource, (Class<T>) t.getClass());
        } catch (JsonProcessingException ex) {
            logger.error("Unable to clone {}.", t.getClass().getName(), ex);
            throw new UnrecoverableException("Unable to clone object", ex);
        }
    }

    /**
     * @param <T>
     *      class of input object
     * @param t
     *      parsed tender
     * @return checksum of the given parsed tender
     *
     * @throws UnrecoverableException if an I/O error occurs
     */
    public static <T> String checksum(final T t) {
        try {
            return DigestUtils.sha256Hex(mapper.writeValueAsString(t));
        } catch (IOException ex) {
            logger.error("Unable to get checksum of {}.", t.getClass().getName(), ex);
            throw new UnrecoverableException("Unable to get checksum", ex);
        }
    }

    /**
     * @param t
     *      parsed tender
     * @return checksum of parsed tender
     */
    public static String checksumParsedTender(final ParsedTender t) {
        ParsedTender clone = clone(t);
        // remove not suitable data for checksum
        clone.setRawObjectId(null);
        clone.setCreatedRaw(null);
        clone.setHash(null);
        clone.setId(null);
        clone.setCreated(null);
        clone.setModified(null);
        clone.setCreatedBy(null);
        clone.setModifiedBy(null);
        clone.setCreatedByVersion(null);
        clone.setModifiedByVersion(null);
        clone.setData(null);
        clone.setMetaData(null);
        clone.setPersistentId(null);
        clone.setProcessingOrder(null);

        return checksum(clone);
    }

    /**
     * @param parsed
     *      list of parsed tenders
     * @param existing
     *      list of existing checksums
     * @return reduced list of parsed tenders
     */
    public static List<ParsedTender> reduceParsedTendersByChecksum(final List<ParsedTender> parsed, final List<String> existing) {
        if (!existing.isEmpty()) {
            Iterator<ParsedTender> i = parsed.iterator();
            while (i.hasNext()) {
                ParsedTender t = i.next();
                if (existing.contains(t.getHash())) {
                    i.remove();
                    logger.info("Parsed tender with hash {} already parsed", t.getHash());
                }
            }
        }

        return parsed;
    }
}
