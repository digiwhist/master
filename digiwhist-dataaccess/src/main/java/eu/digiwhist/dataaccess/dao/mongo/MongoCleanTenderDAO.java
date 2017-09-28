package eu.digiwhist.dataaccess.dao.mongo;

import eu.dl.dataaccess.dao.CleanTenderDAO;
import eu.dl.dataaccess.dao.mongo.GenericMongoDAO;
import eu.dl.dataaccess.dto.clean.CleanTender;

import java.time.LocalDate;
import java.util.List;

/**
 * Clean Tender DAO implementation for MongoDB.
 */
public class MongoCleanTenderDAO extends GenericMongoDAO<CleanTender> implements CleanTenderDAO<CleanTender> {
    private static final String CLEAN_TENDER_COLLECTION_NAME = "cleanTender";

    @Override
    protected final Class<CleanTender> getDTOClass() {
        return CleanTender.class;
    }

    @Override
    protected final String getCollectionName() {
        return CLEAN_TENDER_COLLECTION_NAME;
    }

    @Override
    public final CleanTender getEmptyInstance() {
        return new CleanTender();
    }

    @Override
    public final List<CleanTender> getByCountry(final String countryCode, final Integer page) {
        return null;
    }

    @Override
    public final List<String> getIncludedPublicationSourceIds(final LocalDate date) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
