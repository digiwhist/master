package eu.digiwhist.dataaccess.dao.mongo;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dao.MasterTenderDAO;
import eu.dl.dataaccess.dao.mongo.GenericMongoDAO;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import java.time.LocalDateTime;
import org.mongojack.DBQuery;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Mastered tender DAO implementation for MongoDB.
 */
class MongoMasterTenderDAO extends GenericMongoDAO<MasterTender> implements MasterTenderDAO<MasterTender> {
    private static final String MASTERED_TENDER_COLLECTION_NAME = "masterTender";

    private static final Integer COMBINED_PAGE_SIZE = 100;

    @Override
    protected final Class<MasterTender> getDTOClass() {
        return MasterTender.class;
    }

    @Override
    protected final String getCollectionName() {
        return MASTERED_TENDER_COLLECTION_NAME;
    }

    @Override
    public final List<MasterTender> getByGroupId(final String groupId) {
        final List<MasterTender> result = collection.find(DBQuery.is("groupId", groupId)).toArray();

        return result;
    }

    @Override
    public MasterTender getEmptyInstance() {
        return new MasterTender();
    }

    /**
     * Adds bodies from master collection to this tender.
     *
     * @param tender
     *         tender to be extended
     */
    private void addMasteredBodiesToTender(final MasterTender tender) {
        tender.setBuyers(getMasterBodies(tender.getBuyers()));

        List<MasterTenderLot> lots = tender.getLots();
        if (lots != null) {
            for (MasterTenderLot lot : lots) {
                List<MasterBid> bids = lot.getBids();
                if (bids != null) {
                    for (MasterBid bid : bids) {
                        bid.setBidders(getMasterBodies(bid.getBidders()));
                    }
                }
                lot.setBids(bids);
            }
            tender.setLots(lots);
        }
    }

    /**
     * Gets master bodies.
     *
     * @param bodies
     *         the master bodies will be retrived for this set
     *
     * @return master bodies
     */
    private List<MasterBody> getMasterBodies(final List<MasterBody> bodies) {
        MasterBodyDAO<MasterBody> bodyDao = DAOFactory.getDAOFactory().getMasterBodyDAO(workerName, workerVersion);
        List<MasterBody> masteredBodies = new ArrayList<MasterBody>();
        for (MasterBody body : bodies) {
            List<MasterBody> masterBodies = bodyDao.getByGroupId(body.getGroupId());
            if (!masterBodies.isEmpty()) {
                masteredBodies.addAll(masterBodies);
            }
        }

        if (masteredBodies.isEmpty()) {
            return null;
        } else {
            return masteredBodies;
        }
    }

    @Override
    public final List<MasterTender> getByGroupIds(final Collection<String> groupIds) {
        return null;
    }

    @Override
    public final List<MasterTender> getByCountry(final String countryCode, final Integer page) {
        return null;
    }

    @Override
    public final List<String> getIdsBySourceAndVersion(final String name, final String version) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public final List<MasterTender> getByCountry(final String countryCode, final Integer page, final String source) {
        return null;
    }


    @Override
    public final List<MasterTender> getModifiedAfter(final LocalDateTime timestamp, final String modifiedBy,
        final Integer page, final boolean opentender) {
        return null;
    }

    @Override
    public final List<MasterTender> getByCountry(final String countryCode, final Integer page, final String source,
        final boolean opentender) {
        return null;
    }
}
