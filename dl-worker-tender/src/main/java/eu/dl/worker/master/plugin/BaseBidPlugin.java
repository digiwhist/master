package eu.dl.worker.master.plugin;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Bid Mastering plugin. Matches bids of matched tender lots and creates master record for each bid.
 *
 * @param <T>
 *         implementation type (class) for Matched Tender Lot
 * @param <W>
 *         implementation type (class) for Master Tender Lot
 * @param <U>
 *         implementation type (class) for Matched Bid
 * @param <X>
 *         implementation type (class) for Master Bid
 * @param <V>
 *         implementation type (class) for Matched Tender (context)
 */
public abstract class BaseBidPlugin<T extends MatchedTenderLot, W extends MasterTenderLot, U extends MatchedBid, X
        extends MasterBid, V extends MatchedTender> extends BaseMatchAndMasterPlugin<T, W, U, X> {

    /**
     * Plugin name.
     */
    public static final String PLUGIN_ID = "bidPlugin";

    @Override
    protected final List<List<U>> getListsForMatching(final List<T> items) {
        return items.stream()
                .map(t -> (List<U>) t.getBidsWithStructuredId())
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    @Override
    protected final List<List<U>> match(final List<List<U>> inputListsForMatching) {
        if (inputListsForMatching == null || inputListsForMatching.isEmpty()) {
            return null;
        }

        // Algorithm: match on bidder.groupId
        // Note: bid can have different bidders and bidder can not be in different bids.

        // groups of bids that belong to each other (same bids from different forms)
        List<List<U>> result = new ArrayList<>();

        List<U> allInputBids = inputListsForMatching.stream().flatMap(List::stream).collect(Collectors.toList());

        for (U inputBid : allInputBids) {
            if (inputBid.getBidders() == null) {
                // bid is alone in group when it has no bidder
                result.add(Arrays.asList(inputBid));
                continue;
            }

            // index of group to which input bid has been matched
            int inputBidGroupIndex = -1;

            // cycle through bidders of actual input bid and try to match to some result (matched) bid group
            for (MatchedBody inputBidder : inputBid.getBidders()) {

                // try to match the input bidder against all the already matched bid groups
                for (int i = 0; i < result.size(); ++i) {
                    List<U> matchedBidGroup = result.get(i);
                    boolean isBidderMatchedAgainstActualBidGroup = false;

                    // cycle through all the bids in actual matched bid group
                    for (U matchedBid : matchedBidGroup) {
                        // if there are no bidders in the matched bid, it cannot be matched
                        if (matchedBid.getBidders() == null) {
                            continue;
                        }

                        // cycle through bidders of matched bid and compare to input bid on group id
                        for (MatchedBody matchedBidder : matchedBid.getBidders()) {
                            if (matchedBidder.getGroupId().equals(inputBidder.getGroupId())) {
                                // if this is not the first match for given input bid, check whether it matches the
                                // same group and if not, throw error
                                if (inputBidGroupIndex != -1 && inputBidGroupIndex != i) {
                                    logger.error("Input group ID {} is contained in different matched bid groups",
                                            inputBidder.getGroupId());
                                    throw new UnrecoverableException(
                                            "Input group ID is contained in different matched bid groups");
                                }
                                isBidderMatchedAgainstActualBidGroup = true;
                                inputBidGroupIndex = i;
                                // once matched, no need to try other bidders from the same matched bid
                                break;
                            }
                        }
                        if (isBidderMatchedAgainstActualBidGroup) {
                            // once matched, do not try to match this bidder against other bids in the same matched
                            // bid group, but continue with other bid groups (to check whether this input bid will
                            // match against multiple different matched bid groups which leads to error)
                            break;
                        }
                    }
                }
            }

            if (inputBidGroupIndex != -1) {
                result.get(inputBidGroupIndex).add(inputBid);
            } else {
                result.add(new ArrayList<>(Arrays.asList(inputBid)));
            }
        }

        return result;
    }

    @Override
    protected final W setFinalList(final W finalItem, final List<X> finalList) {
        return (W) finalItem.setBids(finalList);
    }

    @Override
    protected final X setSourceStructuredIds(final List<U> matchedList, final X masterItem) {
        return (X) masterItem.setSourceBidIds(
                matchedList.stream().map(MatchedBid::getStructuredId).collect(Collectors.toList()));
    }
}
