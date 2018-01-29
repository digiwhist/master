package eu.digiwhist.dataaccess.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

/**
 * This class is able to "populate" master tenders with master bodies. Its handy
 * for cases such a API endpoints etc.
 *
 */
public class PopulateUtils {

    private MasterBodyDAO<MasterBody> masterBodyDao;

    /**
     * Initialisation.
     * 
     * @param masterBodyDAO
     *            master body dao
     *            
     */
    public PopulateUtils(final MasterBodyDAO<MasterBody> masterBodyDAO) {
        this.masterBodyDao = masterBodyDAO;
    }

    /**
     * Populates the master tender with all possible master bodies.
     * 
     * @param tenders
     *            list of tenders
     */
    public final void populateBodies(final List<MasterTender> tenders) {
        HashMap<String, MasterBody> bodies = new HashMap<String, MasterBody>(); 
        
        // get all bodies from tenders first
        for (MasterTender tender : tenders) {
            addBodies(bodies, tender.getAdministrators());
            addBodies(bodies, tender.getApproachedBidders());
            addBodies(bodies, tender.getCandidates());
            addBodies(bodies, tender.getSupervisors());
            addBodies(bodies, tender.getBuyers());

            addBodies(bodies, tender.getOnBehalfOf());
            addBody(bodies, tender.getBidsRecipient());
            addBody(bodies, tender.getFurtherInformationProvider());
            addBody(bodies, tender.getSpecificationsCreator());
            addBody(bodies, tender.getSpecificationsProvider());

            List<MasterTenderLot> lots = tender.getLots();
            if (lots != null) {
                for (MasterTenderLot lot : lots) {
                    List<MasterBid> bids = lot.getBids();
                    if (bids != null) {
                        for (MasterBid bid : bids) {
                            addBodies(bodies, bid.getBidders());
                        }
                    }
                }
            }
        }
        
        populateBodySet(bodies);
                
        for (MasterTender tender : tenders) {
            tender.setAdministrators(getBodies(bodies, tender.getAdministrators()));
            tender.setApproachedBidders(getBodies(bodies, tender.getApproachedBidders()));
            tender.setCandidates(getBodies(bodies, tender.getCandidates()));
            tender.setSupervisors(getBodies(bodies, tender.getSupervisors()));
            tender.setBuyers(getBodies(bodies, tender.getBuyers()));

            tender.setOnBehalfOf(getBodies(bodies, tender.getOnBehalfOf()));
            tender.setBidsRecipient(getBody(bodies, tender.getBidsRecipient()));
            tender.setFurtherInformationProvider(getBody(bodies, tender.getFurtherInformationProvider()));
            tender.setSpecificationsCreator(getBody(bodies, tender.getSpecificationsCreator()));
            tender.setSpecificationsProvider(getBody(bodies, tender.getSpecificationsProvider()));

            List<MasterTenderLot> lots = tender.getLots();
            if (lots != null) {
                for (MasterTenderLot lot : lots) {
                    List<MasterBid> bids = lot.getBids();
                    if (bids != null) {
                        for (MasterBid bid : bids) {
                            bid.setBidders(getBodies(bodies, bid.getBidders()));
                        }
                    }
                    lot.setBids(bids);
                }
                tender.setLots(lots);
            }
        }
    }

    /**
     * Depopulates the master tender, so the bodies will have just group ID.
     *
     * @param tenders
     *            list of tenders
     */
    public final void depopulateBodies(final List<MasterTender> tenders) {
        for (MasterTender tender : tenders) {
            tender.setAdministrators(unsetAllExceptGroupIdInBodies(tender.getAdministrators()));
            tender.setApproachedBidders(unsetAllExceptGroupIdInBodies(tender.getApproachedBidders()));
            tender.setCandidates(unsetAllExceptGroupIdInBodies(tender.getCandidates()));
            tender.setSupervisors(unsetAllExceptGroupIdInBodies(tender.getSupervisors()));
            tender.setBuyers(unsetAllExceptGroupIdInBodies(tender.getBuyers()));

            tender.setOnBehalfOf(unsetAllExceptGroupIdInBodies(tender.getOnBehalfOf()));
            tender.setBidsRecipient(unsetAllExceptGroupIdInBody(tender.getBidsRecipient()));
            tender.setFurtherInformationProvider(unsetAllExceptGroupIdInBody(tender.getFurtherInformationProvider()));
            tender.setSpecificationsCreator(unsetAllExceptGroupIdInBody(tender.getSpecificationsCreator()));
            tender.setSpecificationsProvider(unsetAllExceptGroupIdInBody(tender.getSpecificationsProvider()));

            List<MasterTenderLot> lots = tender.getLots();
            if (lots != null) {
                for (MasterTenderLot lot : lots) {
                    List<MasterBid> bids = lot.getBids();
                    if (bids != null) {
                        for (MasterBid bid : bids) {
                            bid.setBidders(unsetAllExceptGroupIdInBodies(bid.getBidders()));
                        }
                    }
                    lot.setBids(bids);
                }
                tender.setLots(lots);
            }
        }
    }

    /**
     * Depopulates the master bodies, so the bodies will have just group ID.
     *
     * @param bodies
     *            list of bodies
     *
     * @return depopulated master bodies or null when there are no bodies
     */
    private List<MasterBody> unsetAllExceptGroupIdInBodies(final List<MasterBody> bodies) {
        if (bodies == null || bodies.isEmpty()) {
            return null;
        }

        List<MasterBody> result = new ArrayList<>();
        for (MasterBody body : bodies) {
            result.add(unsetAllExceptGroupIdInBody(body));
        }
        return result;
    }

    /**
     * Depopulates the master body, so the body will have just group ID.
     *
     * @param body
     *            body
     *
     * @return depopulated master body or null when there is no body
     */
    private MasterBody unsetAllExceptGroupIdInBody(final MasterBody body) {
        return body == null
                ? null
                : new MasterBody()
                .setGroupId(body.getGroupId());
    }

    /**
     * Adds body to target.
     * 
     * @param target
     *            body will be added here
     * @param body
     *            body to add
     */
    private void addBody(final HashMap<String, MasterBody> target, final MasterBody body) {
        if (body != null) {
            target.put(body.getGroupId(), body);
        }
    }

    /**
     * Adds alll the bodies from a list to target.
     * 
     * @param target
     *            bodies will be added here
     * @param bodies
     *            bodies to be added
     */
    private void addBodies(final HashMap<String, MasterBody> target, final List<MasterBody> bodies) {
        if (bodies != null) {
            for (MasterBody body : bodies) {
                target.put(body.getGroupId(), body);
            }
        }
    }
    
    /**
     * Returns body with given id.
     * @param source source of bodies
     * @param body body
     * @return body
     */
    private MasterBody getBody(final HashMap<String, MasterBody> source, final MasterBody body) {
        if (source == null || body == null) {
            return null;
        }
        return source.get(body.getGroupId());
    }

    /**
     * Returns bodies with given ids.
     * @param source source of bodies
     * @param bodies list of bodies
     * @return list of bodies
     */
    private List<MasterBody> getBodies(final HashMap<String, MasterBody> source, final List<MasterBody> bodies) {
        if (bodies == null || bodies.isEmpty()) {
            return null;
        }
        List<MasterBody> result = new ArrayList<MasterBody>();
        List<String> bodyIds = bodies.stream().map(MasterBody::getGroupId).collect(Collectors.toList());
        for (String id : bodyIds) {
            result.add(source.get(id));
        }
        
        return result;
    }

    /**
     * Populates list of master bodies with just group ids filled in with
     * relevant data.
     * 
     * @param bodies
     *            list to be populated
     * 
     */
    private void populateBodySet(final HashMap<String, MasterBody> bodies) {
        if (bodies == null) {
            return;
        }

        List<String> ids = bodies.values().stream().map(MasterBody::getGroupId).collect(Collectors.toList());
        
        List<MasterBody> storedBodies = masterBodyDao.getByGroupIds(ids);
        
        if (storedBodies != null && !storedBodies.isEmpty()) {
            for (MasterBody storedBody: storedBodies) {
                bodies.put(storedBody.getGroupId(), storedBody);
            }
        }
        
    }
}
