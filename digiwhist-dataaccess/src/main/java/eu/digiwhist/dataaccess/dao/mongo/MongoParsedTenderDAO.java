package eu.digiwhist.dataaccess.dao.mongo;

import eu.dl.dataaccess.dao.ParsedTenderDAO;
import eu.dl.dataaccess.dao.mongo.GenericMongoDAO;
import eu.dl.dataaccess.dto.parsed.ParsedTender;

/**
 * Parsed Tender DAO implementation for MongoDB.
 */
class MongoParsedTenderDAO extends GenericMongoDAO<ParsedTender> implements ParsedTenderDAO<ParsedTender> {
    private static final String PARSED_TENDER_COLLECTION_NAME = "parsedTender";

    @Override
    protected final Class<ParsedTender> getDTOClass() {
        return ParsedTender.class;
    }

    @Override
    protected final String getCollectionName() {
        return PARSED_TENDER_COLLECTION_NAME;
    }

    @Override
    public final ParsedTender getEmptyInstance() {
        return new ParsedTender();
    }
}
