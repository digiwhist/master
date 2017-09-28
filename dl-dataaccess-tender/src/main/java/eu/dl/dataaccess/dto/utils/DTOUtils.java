package eu.dl.dataaccess.dto.utils;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.BaseStructuredId;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.dataaccess.dto.matched.StructuredBidId;
import eu.dl.dataaccess.dto.matched.StructuredLotId;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * This class holds methods for DTO handling.
 *
 * @author Marek Mikes
 */
public final class DTOUtils {
    /**
     * Suppress default constructor for noninstantiability.
     */
    private DTOUtils() {
        throw new AssertionError();
    }

    /**
     * @param tenders
     *         clean tenders
     * @param sourceLotIds
     *         source lot ids
     *
     * @return source lots
     */
    public static List<MatchedTenderLot> getSourceLotsByStructuredIds(final List<MatchedTender> tenders,
                                                                      final List<StructuredLotId> sourceLotIds) {
        List<MatchedTenderLot> result = sourceLotIds.stream()
                .map(sourceLotId -> getSourceLotByStructuredId(tenders, sourceLotId))
                .collect(Collectors.toList());
        return result.isEmpty() ? null : result;
    }

    /**
     * @param tenders
     *         clean tenders
     * @param sourceBidIds
     *         source bid ids
     *
     * @return source lots
     */
    public static List<MatchedTenderLot> getSourceLotsByStructuredBidIds(final List<MatchedTender> tenders,
            final List<StructuredBidId> sourceBidIds) {
        List<MatchedTenderLot> result = sourceBidIds.stream()
                .map(sourceBidId -> getSourceLotByStructuredId(tenders, sourceBidId))
                .collect(Collectors.toList());
        return result.isEmpty() ? null : result;
    }

    /**
     * @param tenders
     *         clean tenders
     * @param sourceBidIds
     *         source bid ids
     *
     * @return source bids
     */
    public static List<MatchedBid> getSourceBidsByStructuredIds(final List<MatchedTender> tenders,
                                                                final List<StructuredBidId> sourceBidIds) {
        List<MatchedBid> result = new ArrayList<>();
        for (StructuredBidId sourceBidId : sourceBidIds) {
            List<MatchedBid> wantedBids = getSourceLotByStructuredId(tenders, sourceBidId).getBids()
                    .stream()
                    .filter(bid -> bid.getBidId().equals(sourceBidId.getBidId()))
                    .collect(Collectors.toList());
            assert wantedBids.size() == 1;
            result.add(wantedBids.get(0));
        }
        return result.isEmpty() ? null : result;
    }

    /**
     * @param tenders
     *         clean tenders
     * @param sourceLotId
     *         source lot id
     *
     * @return source lot
     */
    private static MatchedTenderLot getSourceLotByStructuredId(final List<MatchedTender> tenders,
            final BaseStructuredId sourceLotId) {
        List<MatchedTender> wantedTenders = tenders.stream()
                .filter(tender -> tender.getId().equals(sourceLotId.getTenderId()))
                .collect(Collectors.toList());
        assert wantedTenders.size() == 1;
        List<MatchedTenderLot> wantedLots = wantedTenders.get(0)
                .getLots()
                .stream()
                .filter(lot -> lot.getLotId().equals(sourceLotId.getLotId()))
                .collect(Collectors.toList());
        assert wantedLots.size() == 1;
        return wantedLots.get(0);
    }
    
    /**
     * This method selects one matched tender from a matched tender group based on structured id. 
     * Its handy to get "parent" matched tender from based on lot/bid structured id.
     * 
     * @param tenders
     *         matched tenders
     * @param structuredId
     *         structured id
     *
     * @return matched tender or null if not found
     */
    public static MatchedTender getTenderByStructuredId(final List<MatchedTender> tenders, 
            final BaseStructuredId structuredId) {
        if (tenders == null || structuredId == null) {
            return null;
        }
        
        for (MatchedTender tender : tenders) {
            if (tender.getId().equals(structuredId.getTenderId())) {
                return tender;
            }
        }
        return null;
    }

    /**
     * Method gets publication date of the matched tender.
     *
     * @param matchedTender
     *         matched tender
     *
     * @return publication date of the matched tender
     */
    public static LocalDate getPublicationDate(final MatchedTender matchedTender) {
        if (matchedTender == null) {
            return null;
        }

        for (Publication publication : matchedTender.getPublications()) {
            if (publication != null && publication.getIsIncluded() != null && publication.getIsIncluded()) {
                return publication.getPublicationDate();
            }
        }

        return null;
    }

    /**
     * Method gets publication date of the item (part of tender).
     *
     * @param <T>
     *         part of tender which knows its tender ID
     * @param item
     *         item (part of tender)
     * @param context
     *         context that is necessary for getting publication date.
     *
     * @return publication date of the item (part of tender)
     */
    public static <T extends MasterablePart> LocalDate getPublicationDate(
            final T item, final List<MatchedTender> context) {
        if (item == null || context == null) {
            return null;
        }

        String tenderId = item.getTenderId();
        for (MatchedTender matchedTender : context) {
            if (matchedTender.getId().equals(tenderId) && matchedTender.getPublications() != null) {
                for (Publication publication : matchedTender.getPublications()) {
                    if (publication != null && publication.getIsIncluded() != null && publication.getIsIncluded()) {
                        return publication.getPublicationDate();
                    }
                }
            }
        }

        return null;
    }

    /**
     * Method gets publication form type of the item (part of tender).
     *
     * @param <T>
     *         part of tender which knows its tender ID
     * @param item
     *         item (part of tender)
     * @param context
     *         context that is necessary for getting publication form type.
     *
     * @return publication form type of the tender item (part of tender)
     */
    public static <T extends MasterablePart> PublicationFormType getPublicationFormType(final T item,
                                                                                    final List<MatchedTender> context) {
        if (item == null || context == null) {
            return null;
        }
        
        String tenderId = item.getTenderId();
        for (MatchedTender matchedTender : context) {
            if (matchedTender.getId().equals(tenderId) && matchedTender.getPublications() != null) {
                for (Publication publication : matchedTender.getPublications()) {
                    if (publication != null && publication.getIsIncluded() != null && publication.getIsIncluded()) {
                        return publication.getFormType();
                    }
                }
            }
        }

        return null;
    }
    
    /**
     * Method gets publication form type for the tender.
     *
     * @param cleanTender
     *         tender to check publication type for
     * 
     * @return publication form type of the tender
     */
    public static  PublicationFormType getPublicationFormType(final CleanTender cleanTender) {
        if (cleanTender == null || cleanTender.getPublications() == null) {
            return null;
        }

        for (Publication publication : cleanTender.getPublications()) {
            if (publication != null && publication.getIsIncluded() != null && publication.getIsIncluded()) {
                return publication.getFormType();
            }
        }

        return null;
    }

    /**
     * Checks whether the item is empty or not.
     *
     * @param object item to be checked
     *
     * @return true for empty items
     */
    public static Boolean isEmpty(final Object object) {
        // null is empty
        if (object == null) {
            return true;
        }

        if (object instanceof String) {
            return ((String) object).isEmpty();
        }

        if (object instanceof List<?>) {
            return ((List<?>) object).isEmpty();
        }
        return false;
    }
}
