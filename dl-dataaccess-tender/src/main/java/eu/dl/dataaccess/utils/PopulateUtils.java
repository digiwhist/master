package eu.dl.dataaccess.utils;

import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dao.PlainDocumentDAO;
import eu.dl.dataaccess.dto.PlainDocument;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * This class is able to "populate" master tenders with master bodies. Its handy
 * for cases such a API endpoints etc.
 *
 */
public class PopulateUtils {

    private MasterBodyDAO<MasterBody> masterBodyDao;

    private PlainDocumentDAO plainDocumentDAO;

    private MatchedBodyDAO<MatchedBody> matchedBodyDAO;

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
     * Initialisation.
     *
     * @param plainDocumentDAO
     *            plain document dao
     *
     */
    public PopulateUtils(final PlainDocumentDAO plainDocumentDAO) {
        this.plainDocumentDAO = plainDocumentDAO;
    }

    /**
     * Initialisation.
     *
     * @param matchedBodyDAO
     *            plain matched body dao
     *
     */
    public PopulateUtils(final MatchedBodyDAO matchedBodyDAO) {
        this.matchedBodyDAO = matchedBodyDAO;
    }

    /**
     * Populates the master tender with all possible master bodies.
     *
     * @param tender
     *            tender
     */
    public final void populateBodies(final MasterTender tender) {
        if (tender == null) {
            return;
        }

        List<MasterTender> tenders = Arrays.asList(tender);

        populateBodies(tenders);
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
     * Populates tender with plain documents where possible.
     *
     * @param tender
     *          master tender to be populated
     */
    public final void populatePlainDocuments(final MasterTender tender) {
        populatePlainDocuments(Arrays.asList(tender));
    }

    /**
     * Populates tenders with plain documents where possible.
     *
     * @param tenders
     *          list of master tenders to be populated
     */
    public final void populatePlainDocuments(final List<MasterTender> tenders) {
        if (tenders == null) {
            return;
        }

        List<String> docIds = new ArrayList<>();

        for (MasterTender t : tenders) {
            if (t == null) {
                continue;
            }

            // tender documents
            if (t.getDocuments() != null) {
                t.getDocuments().stream()
                    .map(n -> n.getPlainDocumentId())
                    .filter(Objects::nonNull).filter(n -> !docIds.contains(n))
                    .forEach(n -> docIds.add(n));
            }

            // lot bids documents
            if (t.getLots() != null) {
                t.getLots().stream()
                    .map(MasterTenderLot::getBids).filter(Objects::nonNull).flatMap(List::stream)
                    .map(MasterBid::getDocuments).filter(Objects::nonNull).flatMap(List::stream)
                    .map(n -> n.getPlainDocumentId())
                    .filter(Objects::nonNull).filter(n -> !docIds.contains(n))
                    .forEach(n -> docIds.add(n));
            }
        }

        if (!docIds.isEmpty()) {
            List<PlainDocument> plainDocs = plainDocumentDAO.getByIds(docIds);

            Map<String, PlainDocument> documents = new HashMap<>();
            plainDocs.forEach(d -> documents.put(d.getId(), d));

            for (MasterTender t : tenders) {
                if (t.getDocuments() != null) {
                    t.getDocuments().stream()
                        .filter(n -> n.getPlainDocumentId() != null)
                        .forEach(n -> n.setPlainDocument(documents.get(n.getPlainDocumentId())));
                }

                if (t.getLots() != null) {
                    t.getLots().stream()
                        .map(MasterTenderLot::getBids).filter(Objects::nonNull).flatMap(List::stream)
                        .map(MasterBid::getDocuments).filter(Objects::nonNull).flatMap(List::stream)
                        .filter(n -> n.getPlainDocumentId() != null)
                        .forEach(n -> n.setPlainDocument(documents.get(n.getPlainDocumentId())));
                }
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
        if (bodies == null || bodies.isEmpty()) {
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

    /**
     * Populates the matched tender with all possible matched bodies.
     *
     * @param tenders
     *            list of tenders
     */
    public final void populateMatchedBodies(final List<MatchedTender> tenders) {
        HashMap<String, MatchedBody> bodies = new HashMap<>();

        // get all bodies from tenders first
        for (MatchedTender tender : tenders) {
            addMatchedBodies(bodies, tender.getAdministrators());
            addMatchedBodies(bodies, tender.getApproachedBidders());
            addMatchedBodies(bodies, tender.getCandidates());
            addMatchedBodies(bodies, tender.getSupervisors());
            addMatchedBodies(bodies, tender.getBuyers());

            addMatchedBodies(bodies, tender.getOnBehalfOf());
            addMatchedBody(bodies, tender.getBidsRecipient());
            addMatchedBody(bodies, tender.getFurtherInformationProvider());
            addMatchedBody(bodies, tender.getSpecificationsCreator());
            addMatchedBody(bodies, tender.getSpecificationsProvider());

            List<MatchedTenderLot> lots = tender.getLots();
            if (lots != null) {
                for (MatchedTenderLot lot : lots) {
                    List<MatchedBid> bids = lot.getBids();
                    if (bids != null) {
                        for (MatchedBid bid : bids) {
                            addMatchedBodies(bodies, bid.getBidders());
                        }
                    }
                }
            }
        }

        populateMatchedBodySet(bodies);

        for (MatchedTender tender : tenders) {
            tender.setAdministrators(getMatchedBodies(bodies, tender.getAdministrators()));
            tender.setApproachedBidders(getMatchedBodies(bodies, tender.getApproachedBidders()));
            tender.setCandidates(getMatchedBodies(bodies, tender.getCandidates()));
            tender.setSupervisors(getMatchedBodies(bodies, tender.getSupervisors()));
            tender.setBuyers(getMatchedBodies(bodies, tender.getBuyers()));

            tender.setOnBehalfOf(getMatchedBodies(bodies, tender.getOnBehalfOf()));
            tender.setBidsRecipient(getMatchedBody(bodies, tender.getBidsRecipient()));
            tender.setFurtherInformationProvider(getMatchedBody(bodies, tender.getFurtherInformationProvider()));
            tender.setSpecificationsCreator(getMatchedBody(bodies, tender.getSpecificationsCreator()));
            tender.setSpecificationsProvider(getMatchedBody(bodies, tender.getSpecificationsProvider()));

            List<MatchedTenderLot> lots = tender.getLots();
            if (lots != null) {
                for (MatchedTenderLot lot : lots) {
                    List<MatchedBid> bids = lot.getBids();
                    if (bids != null) {
                        for (MatchedBid bid : bids) {
                            bid.setBidders(getMatchedBodies(bodies, bid.getBidders()));
                        }
                    }
                    lot.setBids(bids);
                }
                tender.setLots(lots);
            }
        }
    }

    /**
     * Adds all the bodies from a list to target.
     *
     * @param target
     *            bodies will be added here
     * @param bodies
     *            bodies to be added
     */
    private void addMatchedBodies(final HashMap<String, MatchedBody> target, final List<MatchedBody> bodies) {
        if (bodies != null) {
            bodies.forEach(b -> addMatchedBody(target, b));
        }
    }

    /**
     * Adds body to target.
     *
     * @param target
     *            body will be added here
     * @param body
     *            body to add
     */
    private void addMatchedBody(final HashMap<String, MatchedBody> target, final MatchedBody body) {
        if (body != null) {
            target.put(body.getId(), body);
        }
    }

    /**
     * Populates list of matched bodies with just group ids filled in with
     * relevant data.
     *
     * @param bodies
     *            list to be populated
     *
     */
    private void populateMatchedBodySet(final HashMap<String, MatchedBody> bodies) {
        if (bodies == null || bodies.isEmpty()) {
            return;
        }

        List<String> ids = bodies.values().stream().map(MatchedBody::getId).collect(Collectors.toList());

        List<MatchedBody> storedBodies = matchedBodyDAO.getByIds(ids);

        if (storedBodies != null && !storedBodies.isEmpty()) {
            for (MatchedBody storedBody: storedBodies) {
                bodies.put(storedBody.getId(), storedBody);
            }
        }

    }

    /**
     * Returns bodies with given ids.
     * @param source source of bodies
     * @param bodies list of bodies
     * @return list of bodies
     */
    private List<MatchedBody> getMatchedBodies(final HashMap<String, MatchedBody> source, final List<MatchedBody> bodies) {
        if (bodies == null || bodies.isEmpty()) {
            return null;
        }
        List<MatchedBody> result = new ArrayList<>();
//        for (MatchedBody body : bodies) {
//          result.add(getMatchedBody(source, body));  
//        }
        bodies.stream().forEach(n -> result.add(getMatchedBody(source, n)));

        return result;
    }

    /**
     * Returns body with given id.
     * @param source source of bodies
     * @param body body
     * @return body
     */
    private MatchedBody getMatchedBody(final HashMap<String, MatchedBody> source, final MatchedBody body) {
        if (source == null || body == null) {
            return null;
        }
        return source.get(body.getId());
    }
}
