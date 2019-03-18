package eu.datlab.worker.pl.parsed;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedTender;

import java.io.IOException;
import java.util.List;

import static eu.datlab.worker.pl.parsed.UZPJsonTenderUtils.parseItems;
import static eu.datlab.worker.pl.parsed.UZPJsonTenderUtils.textValue;


/**
 * Handler for Json notices.
 */
public final class UZPJsonTenderNoticeHandler {

    /**
     * Private constructor for noninstatiability.
     */
    private UZPJsonTenderNoticeHandler() {
    }

    /**
     * Parse award data.
     *
     * @param awardCriteriaString award criteria raw
     * @param parsedTender parsedTender
     */
    public static void parse(final String awardCriteriaString, final ParsedTender parsedTender) {
        ObjectMapper mapper = new ObjectMapper();
        final JsonNode awardCriteriaJson;
        try {
            awardCriteriaJson = mapper.readTree(awardCriteriaString.replaceAll("<.*>", ""));
        } catch (IOException e) {
            throw new UnrecoverableException("Unable to parse JSON from raw data", e);
        }

        List<JsonNode> awardCriteria = parseItems(awardCriteriaJson.findValue("ZP_400_Kryteria"));

        for (JsonNode awardCriterion : awardCriteria) {
            parsedTender.addAwardCriterion(new ParsedAwardCriterion()
                    .setName(textValue("Kryteria", awardCriterion))
                    .setWeight(textValue("Znaczenie", awardCriterion))
            );
        }
    }
}
