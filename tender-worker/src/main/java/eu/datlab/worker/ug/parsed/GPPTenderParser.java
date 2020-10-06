package eu.datlab.worker.ug.parsed;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.OCDSReleaseTag;
import eu.dl.dataaccess.dto.ocds.OCDSOrganization;
import eu.dl.dataaccess.dto.ocds.OCDSRelease;
import eu.dl.dataaccess.dto.ocds.OCDSReleasePackage;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.clean.utils.DateUtils;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

/**
 * Tender parser for Uganda.
 *
 * @author Tomas Mrazek
 */
public class GPPTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    private final ObjectMapper mapper;
    /**
     * LocalDateTime deserializer.
     */
    class GPPLocalDateTimeDeserializer extends StdDeserializer<LocalDateTime> {
        private List<DateTimeFormatter> formatters = Arrays.asList(DateTimeFormatter.ISO_OFFSET_DATE_TIME,
            DateTimeFormatter.ofPattern("uuuu-M-d H:m:s"));

        /**
         * Default constructor.
         */
        GPPLocalDateTimeDeserializer() {
            this(null);
        }

        /**
         * @param vc
         *      class of value item
         */
        GPPLocalDateTimeDeserializer(final Class<?> vc) {
            super(vc);
        }

        @Override
        public LocalDateTime deserialize(final JsonParser parser, final DeserializationContext context) throws IOException {
            return DateUtils.cleanDateTime(parser.getText(), formatters);
        }
    }

    /**
     * Object mapper initialization.
     */
    public GPPTenderParser() {
        super();

        JavaTimeModule module = new JavaTimeModule();
        module.addDeserializer(LocalDateTime.class, new GPPLocalDateTimeDeserializer());

        mapper = new ObjectMapper();
        mapper.enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS);
        mapper.registerModule(module);
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }

    @Override
    public final List<ParsedTender> parse(final RawData rawData) {
        HashMap<String, Object> metaData = rawData.getMetaData();

        String tagString = metaData != null ? (String) metaData.get("tag") : null;
        if (tagString == null) {
            logger.error("Raw message {} doesn't include OCDS release tag", rawData.getId());
            throw new UnrecoverableException("OCDS release tag is empty");
        }

        OCDSReleaseTag tag = OCDSReleaseTag.valueOf(tagString.toUpperCase());

        OCDSReleasePackage releasePackage;
        try {
            releasePackage = mapper.readValue(rawData.getSourceData(), OCDSReleasePackage.class);
        } catch (IOException ex) {
            logger.error("Unable to parse JSON because of", ex);
            throw new UnrecoverableException("Unable to parse JSON", ex);
        }

        List<ParsedTender> parsedTenders = new ArrayList<>();
        for (OCDSRelease r: releasePackage.getReleases()) {
            List<OCDSOrganization> bodies = r.getParties();

            ParsedTender tender = new ParsedTender()
                .addPublication(new ParsedPublication()
                    .setPublicationDate(GPPParserUtils.datetimeToString(r.getDate()))
                    .setSource(PublicationSources.UG_GPP)
                    .setIsIncluded(true)
                    .setSourceFormType(tag.name())
                    .setLanguage(r.getLanguage())
                    .setSourceTenderId(r.getOcid())
                    .setSourceId(r.getId())
                    .setMachineReadableUrl(rawData.getSourceUrl().toString()))
                .addBuyer(GPPParserUtils.getBody(r.getBuyer(), bodies));

            switch (tag) {
                case CONTRACT:
                    tender = GPPContractHandler.parse(tender, r);
                    break;
                case PLANNING:
                    tender = GPPPlanningHandler.parse(tender, r);
                    break;
                case TENDER:
                    tender = GPPTenderHandler.parse(tender, r);
                    break;
                case AWARD:
                    tender = GPPAwardHandler.parse(tender, r);
                    break;
                case IMPLEMENTATION:
                    tender = GPPImplementationHandler.parse(tender, r);
                    break;
                default:
                    logger.error("Raw message {} has unsupported OCDS release tag '{}'", r.getId(), tag);
                    throw new UnrecoverableException("Unsupported OCDS release tag");
            }

            parsedTenders.add(tender);
        }

        return parsedTenders;
    }



    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "UG";
    }

    @Override
    protected final List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }
}
