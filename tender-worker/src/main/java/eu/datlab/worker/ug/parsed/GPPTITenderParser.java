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
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;

import java.io.IOException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

/**
 * Tender parser for Uganda - transparency international data.
 *
 * @author Tomas Mrazek
 */
public class GPPTITenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    private final ObjectMapper mapper;

    /**
     * Default constructor with initialization of the OCDS release deserialization.
     */
    public GPPTITenderParser() {
        JavaTimeModule module = new JavaTimeModule();
        module.addDeserializer(LocalDateTime.class, new GPPTILocalDateTimeDeserializer());

        mapper = new ObjectMapper();
        mapper.enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS);
        mapper.registerModule(module);
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }

    /**
     * LocalDateTime deserializer.
     */
    class GPPTILocalDateTimeDeserializer extends StdDeserializer<LocalDateTime> {
        /**
         * Default constructor.
         */
        GPPTILocalDateTimeDeserializer() {
            this(null);
        }

        /**
         * @param vc
         *      class of value item
         */
        GPPTILocalDateTimeDeserializer(final Class<?> vc) {
            super(vc);
        }

        @Override
        public LocalDateTime deserialize(final JsonParser parser, final DeserializationContext context) throws IOException {
            return LocalDateTime.ofInstant(Instant.ofEpochMilli(Long.valueOf(parser.getText())), ZoneOffset.UTC);
        }
    }


    @Override
    public final List<ParsedTender> parse(final RawData rawData) {
        OCDSRelease release;
        try {
            release = mapper.readValue(rawData.getSourceData(), OCDSRelease.class);
        } catch (IOException ex) {
            logger.error("Unable to parse OCDS release from JSON because of", ex);
            throw new UnrecoverableException("Unable to parse JSON", ex);
        }

        List<OCDSOrganization> bodies = release.getParties();

        Map<OCDSReleaseTag, BiFunction<ParsedTender, OCDSRelease, ParsedTender>> handlers = new HashMap<>();
        handlers.put(OCDSReleaseTag.CONTRACT, GPPContractHandler::parse);
        handlers.put(OCDSReleaseTag.PLANNING, GPPPlanningHandler::parse);
        handlers.put(OCDSReleaseTag.TENDER, GPPTenderHandler::parse);
        handlers.put(OCDSReleaseTag.AWARD, GPPAwardHandler::parse);
        handlers.put(OCDSReleaseTag.IMPLEMENTATION, GPPImplementationHandler::parse);

        List<ParsedTender> tenders = new ArrayList<>();

        for (Map.Entry<OCDSReleaseTag, BiFunction<ParsedTender, OCDSRelease, ParsedTender>> item : handlers.entrySet()) {
            String tag = item.getKey().toString();

            ParsedTender t = new ParsedTender()
                .addPublication(new ParsedPublication()
                    .setPublicationDate(GPPParserUtils.datetimeToString(release.getDate()))
                    .setSource(PublicationSources.UG_GPP_TI)
                    .setIsIncluded(true)
                    .setSourceFormType(tag)
                    .setFormType(tag)
                    .setLanguage(release.getLanguage())
                    .setSourceTenderId(release.getId()))
                .addBuyer(GPPParserUtils.getBody(release.getBuyer(), bodies));

            // parse OCDS tag specific data
            t = item.getValue().apply(t, release);

            tenders.add(t);
        }

        return tenders;
    }



    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "UG";
    }
}
