package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Tender Lot Mastering plugin. Matches lots of matched tenders and creates master record for each lot.
 *
 * @param <T>
 *         implementation type (class) for Matched Tender
 * @param <W>
 *         implementation type (class) for Master Tender
 * @param <U>
 *         implementation type (class) for Matched Tender Lot
 * @param <X>
 *         implementation type (class) for Master Tender Lot
 */
public abstract class BaseTenderLotPlugin<T extends MatchedTender, W extends MasterTender, U extends
        MatchedTenderLot, X extends MasterTenderLot> extends BaseMatchAndMasterPlugin<T, W, U, X> {
    /**
     * Plugin name.
     */
    public static final String PLUGIN_ID = "tenderLotPlugin";

    /**
     * This class contains matched tender lot and indices to input list of match method. It is important during matching
     * process where we want to know its parent tender and we want to avoid situation where two lots from one tender are
     * matched.
     * todo: consider refactoring - deletion of this class and using StructuredLotId class.
     */
    private class MatchedTenderLotOverview {
        private int tenderIndex;
        private int lotIndex;
        private U lot;

        /**
         * Private constructor to make this class static.
         */
        private MatchedTenderLotOverview() {
        }

        /**
         * Constructor to initialise the class.
         *
         * @param tenderIndex
         *         index of matched tender lot in list of tenders
         * @param lotIndex
         *         index of matched tender lot in list of lots
         * @param lot
         *         lot
         */
        MatchedTenderLotOverview(final int tenderIndex, final int lotIndex, final U lot) {
            this.tenderIndex = tenderIndex;
            this.lotIndex = lotIndex;
            this.lot = lot;
        }

        /**
         * @return tenderIndex
         */
        public int getTenderIndex() {
            return tenderIndex;
        }

        /**
         * @return lotIndex
         */
        public int getLotIndex() {
            return lotIndex;
        }

        /**
         * @return lot
         */
        public U getLot() {
            return lot;
        }
    }

    /**
     * This class contains information about two lots and its matching ratio.
     */
    private class MatchedTenderLotPairRatio {
        private MatchedTenderLotOverview lot1Overview;
        private MatchedTenderLotOverview lot2Overview;
        private Double matchingRatio;

        /**
         * Private constructor to make this class static.
         */
        private MatchedTenderLotPairRatio() {
        }

        /**
         * Constructor to initialise the class.
         *
         * @param lot1Overview
         *         information about first lot
         * @param lot2Overview
         *         information about second lot
         * @param matchingRatio
         *         matching ratio of the two lots
         */
        MatchedTenderLotPairRatio(final MatchedTenderLotOverview lot1Overview,
                                  final MatchedTenderLotOverview lot2Overview,
                                  final Double matchingRatio) {
            this.lot1Overview = lot1Overview;
            this.lot2Overview = lot2Overview;
            this.matchingRatio = matchingRatio;
        }

        /**
         * @return information about first lot
         */
        public MatchedTenderLotOverview getLot1Overview() {
            return lot1Overview;
        }

        /**
         * @return information about second lot
         */
        public MatchedTenderLotOverview getLot2Overview() {
            return lot2Overview;
        }

        /**
         * @return matching ratio
         */
        public Double getMatchingRatio() {
            return matchingRatio;
        }
    }

    /**
     * This comparator helps to order ratios in the ratio value order.
     */
    final class RatioComparator implements Comparator<MatchedTenderLotPairRatio> {
        @Override
        public int compare(final MatchedTenderLotPairRatio o1, final MatchedTenderLotPairRatio o2) {
            return o1.getMatchingRatio().compareTo(o2.getMatchingRatio());
        }
    }

    /**
     * This class represents matched group of lots.
     */
    private class MatchedTenderLotOverviewGroup {
        private HashMap<String, MatchedTenderLotOverview> lotOverviews;

        /**
         * Private constructor to make this class static.
         */
        private MatchedTenderLotOverviewGroup() {
        }

        /**
         * Constructor to initialise the class.
         *
         * @param lotOverviews
         *         list of information about lots from on group
         */
        MatchedTenderLotOverviewGroup(final List<MatchedTenderLotOverview> lotOverviews) {
            this.lotOverviews = new HashMap<String, MatchedTenderLotOverview>();
            for (MatchedTenderLotOverview lotOverview : lotOverviews) {
                this.lotOverviews.put(getKey(lotOverview), lotOverview);
            }
        }

        /**
         * Adds information about lot to group.
         *
         * @param lotOverview
         *          information about lot
         */
        void addLotOverview(final MatchedTenderLotOverview lotOverview) {
            this.lotOverviews.put(getKey(lotOverview), lotOverview);
        }

        /**
         * Checks whether lot is part of this group.
         *
         * @param lotOverview
         *          information about lot
         *
         * @return true if lot is part of this group; otherwise false
         */
        boolean contains(final MatchedTenderLotOverview lotOverview) {
            return lotOverviews.containsKey(getKey(lotOverview));
        }

        /**
         * Joins another group with this group.
         *
         * @param group
         *          group to be joined
         */
        void join(final MatchedTenderLotOverviewGroup group) {
            lotOverviews.putAll(group.getLotOverviews());
        }

        /**
         * Gets lots contained in this group.
         *
         * @return lots contained in this group
         */
        public Map<String, MatchedTenderLotOverview> getLotOverviews() {
            return lotOverviews;
        }

        /**
         * Checks whether group contains tender.
         *
         * @param tenderIndex
         *          tender index
         *
         * @return true if group contains tender; otherwise false
         */
        boolean hasLotFromTender(final int tenderIndex) {
            return lotOverviews.values()
                    .stream()
                    .anyMatch(lo -> lo.getTenderIndex() == tenderIndex);
        }
        
        /**
         * Creates map key from the lotOverview.
         * 
         * @param lotOverview key will be created from this overview
         * @return key
         */
        private String getKey(final MatchedTenderLotOverview lotOverview) {
            return lotOverview.getTenderIndex() + "_" + lotOverview.getLotIndex();
        }
    }

    @Override
    protected final List<List<U>> getListsForMatching(final List<T> items) {
        return items.stream().map(t -> (List<U>) t.getLotsWithStructuredId()).filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    /**
     * Lots matching.
     */
    @Override
    protected final List<List<U>> match(final List<List<U>> lotLists) {
        if (lotLists.isEmpty()) {
            return null;
        }

        // if all the tenders have one lot only, skip the algorithm and match them as one lot without any comparisons
        if (lotLists.stream().allMatch(lotList -> lotList.size() == 1)) {
            return Arrays.asList(lotLists.stream().flatMap(List::stream).collect(Collectors.toList()));
        }

        // calculate matching ratio MR for each cross tender lot-lot pair
        List<MatchedTenderLotPairRatio> lotPairRatios = new ArrayList<>();
        for (int firstLotTenderIndex = 0; firstLotTenderIndex < lotLists.size() - 1; ++firstLotTenderIndex) {
            List<U> lotList1 = lotLists.get(firstLotTenderIndex);
            for (int firstLotIndex = 0; firstLotIndex < lotList1.size(); ++firstLotIndex) {
                // we can get the first lot to compare
                U firstLot = lotList1.get(firstLotIndex);
                for (int secondLotTenderIndex = firstLotTenderIndex + 1; secondLotTenderIndex < lotLists.size();
                     ++secondLotTenderIndex) {
                    List<U> lotList2 = lotLists.get(secondLotTenderIndex);
                    for (int secondLotIndex = 0; secondLotIndex < lotList2.size(); ++secondLotIndex) {
                        // we can get the second lot to compare
                        U secondLot = lotList2.get(secondLotIndex);

                        // calculating matching ratio MR
                        Double matchingRatio = calculateMatchingRatio(firstLot, secondLot, lotLists);

                        lotPairRatios.add(new MatchedTenderLotPairRatio(
                                new MatchedTenderLotOverview(firstLotTenderIndex, firstLotIndex, firstLot),
                                new MatchedTenderLotOverview(secondLotTenderIndex, secondLotIndex, secondLot),
                                matchingRatio));
                    }
                }
            }
        }

        // sort all tender lot-lot pairs according to matching ratio MR
        lotPairRatios = lotPairRatios.stream().sorted(new RatioComparator()).collect(Collectors.toList());

        // visit all pairs from the end (with the higher score) to begin and create matched lot groups. Rules:
        //   MR >= 0.5 ... match lots (they are in one group)
        //   MR < 0.5  ... do not match lots (each lot is in separated group)
        List<MatchedTenderLotOverviewGroup> lotOverviewGroups = new ArrayList<>();
        while (!lotPairRatios.isEmpty()) {
            // get pair with the higher score from the end of sorted list
            MatchedTenderLotPairRatio lotPairRatio = lotPairRatios.remove(lotPairRatios.size() - 1);
            MatchedTenderLotOverview lot1Overview = lotPairRatio.getLot1Overview();
            MatchedTenderLotOverview lot2Overview = lotPairRatio.getLot2Overview();

            // two lots of the pair belong to some group(s), because the pair is valid (invalid pairs were previously
            // deleted)

            if (lotPairRatio.getMatchingRatio().compareTo(new Double(0.5)) >= 0) {
                boolean createNewGroup = true;

                for (int i = 0; i < lotOverviewGroups.size(); ++i) {
                    MatchedTenderLotOverviewGroup lotOverviewGroup = lotOverviewGroups.get(i);

                    if (!lotOverviewGroup.contains(lot1Overview) && !lotOverviewGroup.contains(lot2Overview)) {
                        // no lot of the pair is in the group -> try another one
                        continue;
                    }

                    // we know that the pair has to be joined to the group or ignored (when it cause invalid group or
                    // the pair is already in group)

                    if (lotOverviewGroup.contains(lot1Overview) && !lotOverviewGroup.contains(lot2Overview)) {
                        if (!lotOverviewGroup.hasLotFromTender(lot2Overview.getTenderIndex())) {
                            addLotToGroup(lot2Overview, lotOverviewGroups, i);
                        }
                    } else if (!lotOverviewGroup.contains(lot1Overview) && lotOverviewGroup.contains(lot2Overview)){
                        if (!lotOverviewGroup.hasLotFromTender(lot1Overview.getTenderIndex())) {
                            addLotToGroup(lot1Overview, lotOverviewGroups, i);
                        }
                    }

                    createNewGroup = false;
                    break;
                }

                if (createNewGroup) {
                    lotOverviewGroups.add(new MatchedTenderLotOverviewGroup(
                            new ArrayList<>(Arrays.asList(lot1Overview, lot2Overview))));
                }
            } else { // MR < 0.5
                // add each lot to separated group if it is not already in some other group
                if (lotOverviewGroups.stream().noneMatch(g -> g.contains(lot1Overview))) {
                    lotOverviewGroups.add(
                            new MatchedTenderLotOverviewGroup(new ArrayList<>(Arrays.asList(lot1Overview))));
                }
                if (lotOverviewGroups.stream().noneMatch(g -> g.contains(lot2Overview))) {
                    lotOverviewGroups.add(
                            new MatchedTenderLotOverviewGroup(new ArrayList<>(Arrays.asList(lot2Overview))));
                }
            }
        }

        // convert groups structure which will be returned
        List<List<U>> matchedLots = lotOverviewGroups
                .stream()
                .map(g -> g.getLotOverviews().values()
                        .stream()
                        .map(l -> l.getLot())
                        .collect(Collectors.toList()))
                .collect(Collectors.toList());

        // create group with one lot if the lot is not in matched groups
        for (int i = 0; i < lotLists.size(); ++i) {
            List<U> lotList = lotLists.get(i);
            for (int j = 0; j < lotList.size(); ++j) {
                U lot = lotList.get(j);
                if (!matchedLots
                        .stream()
                        .anyMatch(lotGroup -> lotGroup
                                .stream()
                                .anyMatch(l -> l.equals(lot)))) {
                    matchedLots.add(Arrays.asList(lot));
                }
            }
        }

        assert matchedLots.stream().mapToInt(l -> l.size()).sum() == lotLists.stream().mapToInt(l -> l.size()).sum()
                : "Number of matched lots has to be equal to number of input lot lists.";

        return matchedLots;
    }

    @Override
    protected final W setFinalList(final W finalItem, final List<X> finalList) {
        return (W) finalItem.setLots(finalList);
    }

    @Override
    protected final X setSourceStructuredIds(final List<U> matchedList, final X masterItem) {
        return (X) masterItem.setSourceLotIds(
                matchedList.stream().map(MatchedTenderLot::getStructuredId).collect(Collectors.toList()));
    }

    /**
     * Calculates matching ratio of two lots.
     *
     * @param firstLot
     *          first matched lot
     * @param secondLot
     *          second matched lot
     * @param items
     *          list of matched lot lists (each matched lot list represents lots of one matched tender)
     * @return matching ratio
     */
    protected abstract Double calculateMatchingRatio(U firstLot, U secondLot, List<List<U>> items);

    /**
     * Adds lot to matched group of lots. The lot can be part of some other group (it is checked only with groups
     * after the group) so the group may be joined with another one.
     *
     * @param lot
     *          lot to be added
     * @param groups
     *          matched groups of lots
     * @param groupIndex
     *          index to matched groups of lots to get the right group of lots
     */
    private void addLotToGroup(final MatchedTenderLotOverview lot,
                               final List<MatchedTenderLotOverviewGroup> groups,
                               final int groupIndex) {
        // add lot which is not in group
        groups.get(groupIndex).addLotOverview(lot);

        // it is possible that the pair joined two groups, so try to join
        for (int j = groupIndex + 1; j < groups.size(); ++j) {
            MatchedTenderLotOverviewGroup lotOverviewGroup = groups.get(j);
            if (lotOverviewGroup.contains(lot)) {
                // JOIN THE TWO GROUPS
                groups.get(groupIndex).join(lotOverviewGroup);
                groups.remove(j);
                break;
            }
        }
    }
}