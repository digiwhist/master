package eu.datlab.worker.master.plugin;

import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Tender Lot Mastering plugin. Matches lots of matched tenders and creates master record for each lot. For dps calculateMatchingRatio
 * returns score 0.
 */
public final class TenderLotDPSPlugin extends BaseDatlabTenderLotPlugin {

    /**
     * Calculates matching ratio MR of two lots. Formula:
     * MR = MS / C
     * where
     *  MS is the matching score - sum of scores from all the comparisons
     *  C is the number of comparisons - number of comparisons on non-null values (null values are not compared)
     *
     * Compares on following attributes:
     * - bidsCount (exact match 1, otherwise 0)
     * - selectionMethod (exact match 1, otherwise 0)
     * - contractSignatureDate (exact match 1, otherwise 0)
     * - estimatedPrice.netAmountEur (exact match 1, otherwise 0)
     * - CPVs[isMain=true].code (exact match 1, otherwise 0)
     * - title (exact match 1, otherwise 0)
     * - bids[isWinning=true].bidders.groupId (exact match 1, otherwise 0)
     * - contractNumber (exact match 1, otherwise 0)
     * - lotNumber (exact match 2, otherwise 0)
     * - positionOnPage
     *   (1 - ((|lot1.positionOnPage - lot2.positionOnPage|) / (N - 1))) * k,
     *   where
     *   N is number of lots ( maximum from the matched tenders )
     *   k is a constant:
     *     k = 1 if all the tenders have the same number of lots
     *     k = 0.9 otherwise
     *
     * @param firstLot
     *          first matched lot
     * @param secondLot
     *          second matched lot
     * @param items
     *          list of matched lot lists (each matched lot list represents lots of one matched tender)
     * @return matching ratio
     */
    @Override
    protected Double calculateMatchingRatio(final MatchedTenderLot firstLot, final MatchedTenderLot secondLot,
                                            final List<List<MatchedTenderLot>> items) {
        if (Boolean.TRUE.equals(getFinalItem().getIsDps())) {
            return 0.0;
        }

        Double matchingScore = (double) 0.0;
        int numberOfComparisons = 0;

        if (firstLot.getBidsCount() != null && secondLot.getBidsCount() != null) {
            if (firstLot.getBidsCount().equals(secondLot.getBidsCount())) {
                matchingScore++;
            }
            numberOfComparisons++;
        }

        if (firstLot.getSelectionMethod() != null && secondLot.getSelectionMethod() != null) {
            if (firstLot.getSelectionMethod().equals(secondLot.getSelectionMethod())) {
                matchingScore++;
            }
            numberOfComparisons++;
        }

        if (firstLot.getContractSignatureDate() != null && secondLot.getContractSignatureDate() != null) {
            if (firstLot.getContractSignatureDate().equals(secondLot.getContractSignatureDate())) {
                matchingScore++;
            }
            numberOfComparisons++;
        }

        if (firstLot.getEstimatedPrice() != null && firstLot.getEstimatedPrice().getNetAmount() != null
            && secondLot.getEstimatedPrice() != null && secondLot.getEstimatedPrice().getNetAmount() != null) {
            if (firstLot.getEstimatedPrice().getNetAmount().equals(
                secondLot.getEstimatedPrice().getNetAmount())) {
                matchingScore++;
            }
            numberOfComparisons++;
        }

        if (firstLot.getTitle() != null && secondLot.getTitle() != null) {
            if (firstLot.getTitle().equals(secondLot.getTitle())) {
                matchingScore++;
            }
            numberOfComparisons++;
        }

        if (firstLot.getContractNumber() != null && secondLot.getContractNumber() != null) {
            if (firstLot.getContractNumber().equals(secondLot.getContractNumber())) {
                matchingScore++;
            }
            numberOfComparisons++;
        }

        if (firstLot.getLotNumber() != null && secondLot.getLotNumber() != null) {
            if (firstLot.getLotNumber().equals(secondLot.getLotNumber())) {
                matchingScore = matchingScore + 2;
            }
            numberOfComparisons++;
        }

        if ((numberOfComparisons > 3 && matchingScore == 0)
            || (numberOfComparisons > 5 && matchingScore == 1)) {
            return 0.0;
        }

        if (firstLot.getCpvs() != null && secondLot.getCpvs() != null) {
            List<CPV> mainCpvsOfFirstLot = firstLot.getCpvs()
                .stream()
                .filter(cpv -> cpv.getIsMain() != null && cpv.getIsMain())
                .collect(Collectors.toList());
            assert mainCpvsOfFirstLot.size() <= 1;
            CPV mainCpvOfFirstLot = mainCpvsOfFirstLot.isEmpty() ? null : mainCpvsOfFirstLot.get(0);
            List<CPV> mainCpvsOfSecondLot = firstLot.getCpvs()
                .stream()
                .filter(cpv -> cpv.getIsMain() != null && cpv.getIsMain())
                .collect(Collectors.toList());
            assert mainCpvsOfSecondLot.size() <= 1;
            CPV mainCpvOfSecondLot = mainCpvsOfSecondLot.isEmpty() ? null : mainCpvsOfSecondLot.get(0);
            if (mainCpvOfFirstLot != null && mainCpvOfFirstLot.getCode() != null
                && mainCpvOfSecondLot != null && mainCpvOfSecondLot.getCode() != null) {
                if (mainCpvOfFirstLot.getCode().equals(mainCpvOfSecondLot.getCode())) {
                    matchingScore++;
                }
                numberOfComparisons++;
            }
        }

        if ((numberOfComparisons > 2 && matchingScore == 0)
            || (numberOfComparisons > 4 && matchingScore == 1)
            || (numberOfComparisons > 6 && matchingScore == 2)) {
            return 0.0;
        }

        if (firstLot.getBids() != null && secondLot.getBids() != null) {
            List<MatchedBid> winningBidsOfFirstLot = firstLot.getBids()
                .stream()
                .filter(bid -> bid.getIsWinning() != null && bid.getIsWinning())
                .collect(Collectors.toList());
            assert winningBidsOfFirstLot.size() <= 1;
            MatchedBid winningBidOfFirstLot = winningBidsOfFirstLot.isEmpty() ? null : winningBidsOfFirstLot.get(0);
            List<MatchedBid> winningBidsOfSecondLot = secondLot.getBids()
                .stream()
                .filter(bid -> bid.getIsWinning() != null && bid.getIsWinning())
                .collect(Collectors.toList());
            assert winningBidsOfSecondLot.size() <= 1;
            MatchedBid winningBidOfSecondLot = winningBidsOfSecondLot.isEmpty() ? null : winningBidsOfSecondLot.get(0);
            if (winningBidOfFirstLot != null && winningBidOfFirstLot.getBidders() != null
                && winningBidOfSecondLot != null && winningBidOfSecondLot.getBidders() != null) {
                if (winningBidOfFirstLot.getBidders()
                    .stream()
                    .anyMatch(bidder1 -> winningBidOfSecondLot.getBidders()
                        .stream()
                        .anyMatch(bidder2 -> bidder2.getGroupId().equals(bidder1.getGroupId())))) {
                    matchingScore++;
                }
                numberOfComparisons++;
            }
        }

        if ((numberOfComparisons > 1 && matchingScore == 0)
            || (numberOfComparisons > 3 && matchingScore == 1)
            || (numberOfComparisons > 5 && matchingScore == 2)
            || (numberOfComparisons > 7 && matchingScore == 3)) {
            return 0.0;
        }

        if (firstLot.getPositionOnPage() != null && secondLot.getPositionOnPage() != null) {
            // Add to matching score:
            // (1 - ((lot1.positionOnPage - lot2.positionOnPage) / (N - 1))) * k,
            // where
            // N is number of lots ( maximum from the matched tenders)
            // k is a constant:
            // k = 1 if all the tenders have the same number of lots
            //        k = 0.9 otherwise
            int n = Collections.max(items
                .stream()
                .map(l -> l.size())
                .collect(Collectors.toList()));
            float k = items
                .stream()
                .allMatch(l -> l.size() == n) ? 1.0f : 0.9f;
            matchingScore = matchingScore +
                (1.0f - (Math.abs(firstLot.getPositionOnPage() - secondLot.getPositionOnPage()) / (n - 1.0f))) * k;
            numberOfComparisons++;
        }

        // matching ratio MR = MS / C, where
        //   MS is the matching score - sum of scores from all the comparisons
        //   C is the number of comparisons - number of comparisons on non-null values (null values are not compared)
        return matchingScore / numberOfComparisons;
    }
}
