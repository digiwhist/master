package eu.datlab.worker.py.parsed;

import com.fasterxml.jackson.databind.JsonNode;
import eu.datlab.worker.py.DNCPFormType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static eu.datlab.worker.py.parsed.DNCPParserUtils.getSourceTenderId;
import static eu.datlab.worker.py.parsed.DNCPParserUtils.parseIncludedPublication;
import static eu.datlab.worker.py.parsed.DNCPParserUtils.path;

/**
 * DNCP Planning handler.
 * 
 * @author Tomas Mrazek
 */
public final class DNCPPlanningHandler {

    /**
     * Suppress default constructor.
     */
    private DNCPPlanningHandler() {
    }

    /**
     * @param json
     *      JSON to be parsed
     * @param publicationDate
     *      publication date
     * @param url
     *      human readable url
     * @param metaData
     *      raw message metadata
     * @return list of parsed tenders
     */
    public static List<ParsedTender> parse(final JsonNode json, final String publicationDate, final String url,
                                           final Map<String, Object> metaData) {
        JsonNode release = DNCPParserUtils.path("releases/0", json);

        JsonNode data = path("planning", release);

        ParsedTender t = new ParsedTender()
            .addPublication(parseIncludedPublication(DNCPFormType.PLANNING.name(), publicationDate, url, data)
                // planning node doesn't include id parameter
                .setSourceTenderId(getSourceTenderId(release.path("id").textValue())));

        return Arrays.asList(t);
    }
}
