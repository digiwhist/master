package eu.digiwhist.dataaccess.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import eu.dl.dataaccess.dao.IndicatorDAO;
import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dto.indicator.EntitySpecificIndicator;
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
    
    private IndicatorDAO indicatorDao;

    /**
     * Initialisation.
     * 
     * @param masterBodyDAO
     *            master body dao
     *            
     * @param indicatorDao
     *            indicator dao
     */
    public PopulateUtils(final MasterBodyDAO<MasterBody> masterBodyDAO, final IndicatorDAO indicatorDao) {
        this.masterBodyDao = masterBodyDAO;
        this.indicatorDao = indicatorDao;
    }

    /**
     * Populates the master tender with all possible master bodies.
     * 
     * @param tenders
     *            list of tenders
     * @return tenders with master bodies
     */
    public final List<MasterTender> populateBodies(final List<MasterTender> tenders) {
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

        return tenders;
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
     * Populates the master tender with all possible master bodies.
     * 
     * @param tenders
     *            list of tenders
     * @return tenders with master bodies
     */
    public final List<MasterTender> populateIndicators(final List<MasterTender> tenders) {
        List<String> tenderIds = tenders.stream().map(MasterTender::getId).collect(Collectors.toList());
        List<EntitySpecificIndicator> indicators = indicatorDao.getByEntityIds(tenderIds);
        
        if (indicators == null || indicators.isEmpty()) {
            return tenders;
        }
        
        HashMap<String, List<EntitySpecificIndicator>> indicatorStorage = 
                new HashMap<String, List<EntitySpecificIndicator>>(); 
        
        for (EntitySpecificIndicator indicator: indicators) {
            if (indicatorStorage.containsKey(indicator.getRelatedEntityId())) {
                indicatorStorage.get(indicator.getRelatedEntityId()).add(indicator);
            } else {
                List<EntitySpecificIndicator> list  = new ArrayList<EntitySpecificIndicator>();
                list.add(indicator);
                indicatorStorage.put(indicator.getRelatedEntityId(), list);
            }
        }
        
        for (MasterTender tender : tenders) {
            if (indicators != null && !indicators.isEmpty()) {
                tender.setIndicators(indicatorStorage.get(tender.getId()));
            }
        }

        return tenders;
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
