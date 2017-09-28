package eu.dl.dataaccess.dto.utils;

import java.util.ArrayList;
import java.util.List;

import eu.dl.dataaccess.dto.clean.CleanBid;
import eu.dl.dataaccess.dto.clean.CleanBody;
import eu.dl.dataaccess.dto.clean.CleanTenderLot;
import eu.dl.dataaccess.dto.codetables.BodyType;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;


/**
 * Different helpers for body objects.
 */
public final class InitUtils {

    /**
     * No public constructor.
     */
    protected InitUtils() {
        super();
    }

    /**
     * Transforms list of clean bodies to list of initialized matched bodies.
     * 
     * @param cleanBodies
     *            clean bodies
     * @param role
     *            role of the body on the tender
     * @return list of matched bodies or null in case the entry list is null or
     *         empty
     */
    public static List<MatchedBody> cleanToMatchedBody(final List<CleanBody> cleanBodies, final BodyType role) {
        if (cleanBodies != null && !cleanBodies.isEmpty()) {
            List<MatchedBody> matchedBodies = new ArrayList();
            for (CleanBody cleanBody : cleanBodies) {
                matchedBodies.add(new MatchedBody(cleanBody, role));
            }
            return matchedBodies;
        } else {
            return null;
        }
    }

    /**
     * Transforms list of clean lots to list of initialized matched lots.
     * 
     * @param cleanTenderLots
     *            clean lots
     * @return list of matched tenders or null in case the entry list is null or
     *         empty
     */
    public static List<MatchedTenderLot> cleanToMatchedLot(final List<CleanTenderLot> cleanTenderLots) {
        if (cleanTenderLots != null && !cleanTenderLots.isEmpty()) {
            List<MatchedTenderLot> matchedTenderLots = new ArrayList();
            for (CleanTenderLot cleanTenderLot : cleanTenderLots) {
                matchedTenderLots.add(new MatchedTenderLot(cleanTenderLot));
            }
            return matchedTenderLots;
        } else {
            return null;
        }
    }

    /**
     * Transforms list of clean bids to list of initialized matched bids.
     * 
     * @param cleanBids
     *            clean bids
     * @return list of matched bids or null in case the entry list is null or
     *         empty
     */
    public static List<MatchedBid> cleanToMatchedBid(final List<CleanBid> cleanBids) {
        if (cleanBids != null && !cleanBids.isEmpty()) {
            List<MatchedBid> matchedBids = new ArrayList();
            for (CleanBid cleanBid : cleanBids) {
                matchedBids.add(new MatchedBid(cleanBid));
            }
            return matchedBids;
        } else {
            return null;
        }
    }

    /**
     * Transforms list of matched bodies to list of initialized master bodies.
     * 
     * @param matchedBodies
     *            matched bodies
     * @return list of master bodies or null in case the entry list is null or
     *         empty
     */
    public static List<MasterBody> matchedToMasterBody(final List<MatchedBody> matchedBodies) {
        if (matchedBodies != null && !matchedBodies.isEmpty()) {
            List<MasterBody> masterBodies = new ArrayList();
            for (MatchedBody matchedBody : matchedBodies) {
                masterBodies.add(new MasterBody(matchedBody));
            }
            return masterBodies;
        } else {
            return null;
        }
    }

    /**
     * Transforms matched body to master body.
     *
     * @param matchedBody
     *            matched body
     * @return master body or null in case the entry is null
     */
    public static MasterBody matchedToMasterBody(final MatchedBody matchedBody) {
        return matchedBody == null ? null : new MasterBody(matchedBody);
    }

    /**
     * Transforms list of matched lots to list of initialized master lots.
     * 
     * @param matchedTenderLots
     *            matched lots
     * @return list of master tenders or null in case the entry list is null or
     *         empty
     */
    public static List<MasterTenderLot> matchedToMasterLot(final List<MatchedTenderLot> matchedTenderLots) {
        if (matchedTenderLots != null && !matchedTenderLots.isEmpty()) {
            List<MasterTenderLot> masterTenderLots = new ArrayList();
            for (MatchedTenderLot matchedTenderLot : matchedTenderLots) {
                masterTenderLots.add(new MasterTenderLot(matchedTenderLot));
            }
            return masterTenderLots;
        } else {
            return null;
        }
    }

    /**
     * Transforms list of matched bids to list of initialized master bids.
     * 
     * @param matchedBids
     *            matched bids
     * @return list of master bids or null in case the entry list is null or
     *         empty
     */
    public static List<MasterBid> matchedToMasterBid(final List<MatchedBid> matchedBids) {
        if (matchedBids != null && !matchedBids.isEmpty()) {
            List<MasterBid> masterBids = new ArrayList();
            for (MatchedBid matchedBid : matchedBids) {
                masterBids.add(new MasterBid(matchedBid));
            }
            return masterBids;
        } else {
            return null;
        }
    }
}
