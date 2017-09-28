package eu.digiwhist.dataaccess.dao.mongo;

import eu.digiwhist.dataaccess.dao.ParsedPublicOfficialDAO;
import eu.dl.dataaccess.dao.mongo.GenericMongoDAO;
import eu.dl.dataaccess.dto.parsed.ParsedPublicOfficial;

/**
 * Parsed Public official DAO implementation for MongoDB.
 */
public class MongoParsedPublicOfficialDAO extends GenericMongoDAO<ParsedPublicOfficial> implements
    ParsedPublicOfficialDAO<ParsedPublicOfficial> {

    private static final String PARSED_PUBLIC_OFFICIAL_COLLECTION_NAME = "parsedPublicOfficial";

    @Override
    protected final Class<ParsedPublicOfficial> getDTOClass() {
        return ParsedPublicOfficial.class;
    }

    @Override
    protected final String getCollectionName() {
        return PARSED_PUBLIC_OFFICIAL_COLLECTION_NAME;
    }

    @Override
    public final ParsedPublicOfficial getEmptyInstance() {
        return new ParsedPublicOfficial();
    }
}
