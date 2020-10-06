package eu.datlab.worker.global.parsed;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import eu.datlab.worker.parser.BasePublicOfficialParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedParty;
import eu.dl.dataaccess.dto.parsed.ParsedPublicOfficial;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Public official parsed for Every Politician.
 *
 * @author Michal Riha
 */
public class EveryPoliticianPublicOfficialParser extends BasePublicOfficialParser {

    private static final String PARSER_VERSION = "1";

    @Override
    public final List<ParsedPublicOfficial> parse(final RawData rawPublicOfficial) {
        List<ParsedPublicOfficial> parsedPublicOfficials = new ArrayList<>();

        ObjectMapper mapper = new ObjectMapper();
        try {
            JsonNode jsonData = mapper.readTree(rawPublicOfficial.getSourceData());
            JsonNode arrayOfPublicOfficials = jsonData.get("persons");

            for (JsonNode publicOfficial : arrayOfPublicOfficials) {
                parsedPublicOfficials.add(new ParsedPublicOfficial().setGivenName(
                        getValueFromJsonSafely(publicOfficial, "given_name"))
                        .setFamilyName(getValueFromJsonSafely(publicOfficial, "family_name"))
                        .setFullName(getValueFromJsonSafely(publicOfficial, "name"))
                        .setDateOfBirth(getValueFromJsonSafely(publicOfficial, "birth_date"))
                        .setGender(getValueFromJsonSafely(publicOfficial, "gender"))
                        .setParties(parseParties(jsonData, publicOfficial)));
            }
        } catch (IOException e) {
            logger.error("Crawling failed with exception {}", e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
        return parsedPublicOfficials;
    }

    @Override
    protected final String getVersion() {
        return PARSER_VERSION;
    }

    /**
     * Get text value from JsonNode by parameter.
     *
     * @param jsonNode
     *         JsonNode to be get value from
     * @param parameter
     *         parameter by which choose value
     *
     * @return String value of parameter
     */
    private String getValueFromJsonSafely(final JsonNode jsonNode, final String parameter) {
        if (jsonNode == null || parameter == null) {
            return null;
        }

        JsonNode returnNode = jsonNode.get(parameter);

        if (returnNode == null) {
            return null;
        }

        return returnNode.textValue();
    }

    /**
     * Find all party memberships the public official was or is in.
     *
     * @param jsonData
     *         jsonData to look in
     * @param publicOfficial
     *         publicOfficial for who to look for memberships
     *
     * @return List<ParsedParty> all parties publicOfficial was or is a member.
     */
    private List<ParsedParty> parseParties(final JsonNode jsonData, final JsonNode publicOfficial) {
        String publicOfficialID = getValueFromJsonSafely(publicOfficial, "id");

        if (publicOfficialID == null) {
            return null;
        }

        List<ParsedParty> parties = new ArrayList<>();
        JsonNode arrayOfMembersips = jsonData.get("memberships");

        for (JsonNode membership : arrayOfMembersips) {
            if (publicOfficialID.equals(getValueFromJsonSafely(membership, "person_id"))) {
                parties.add(new ParsedParty().setParty(parsePartyName(jsonData, membership))
                        .setStartDate(getValueFromJsonSafely(membership, "start_date"))
                        .setEndDate(getValueFromJsonSafely(membership, "end_date")));
            }
        }

        return parties;
    }

    /**
     * Get party name against partyID in membership.
     *
     * @param jsonData
     *         jsonData to look for party name in
     * @param membership
     *         membership to get partyID in
     *
     * @return String party name
     */
    private String parsePartyName(final JsonNode jsonData, final JsonNode membership) {
        String onBehalfOfPartyId = getValueFromJsonSafely(membership, "on_behalf_of_id");

        if (onBehalfOfPartyId == null) {
            return null;
        }

        JsonNode parties = jsonData.get("organizations");

        for (JsonNode party : parties) {
            if (onBehalfOfPartyId.equals(getValueFromJsonSafely(party, "id"))) {
                return getValueFromJsonSafely(party, "name");
            }
        }

        return null;
    }

    @Override
    protected final List<ParsedPublicOfficial> postProcessSourceSpecificRules(final List<ParsedPublicOfficial> parsed, final RawData raw) {
        return parsed;
    }
}
