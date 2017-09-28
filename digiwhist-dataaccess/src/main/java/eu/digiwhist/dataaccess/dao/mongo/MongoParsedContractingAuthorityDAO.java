package eu.digiwhist.dataaccess.dao.mongo;

import eu.digiwhist.dataaccess.dao.ParsedContractingAuthorityDAO;
import eu.digiwhist.dataaccess.dto.parsed.ParsedContractingAuthority;
import eu.dl.dataaccess.dao.mongo.GenericMongoDAO;

/**
 * Parsed Contracting Authority DAO implementation for MongoDB.
 */
class MongoParsedContractingAuthorityDAO extends GenericMongoDAO<ParsedContractingAuthority> implements
    ParsedContractingAuthorityDAO<ParsedContractingAuthority> {

    private static final String PARSED_CONTRACTING_AUTHORITY_COLLECTION_NAME = "parsedContractingAuthority";

    @Override
    protected Class<ParsedContractingAuthority> getDTOClass() {
        return ParsedContractingAuthority.class;
    }

    @Override
    protected String getCollectionName() {
        return PARSED_CONTRACTING_AUTHORITY_COLLECTION_NAME;
    }

    @Override
    public ParsedContractingAuthority getEmptyInstance() {
        return new ParsedContractingAuthority();
    }
}
