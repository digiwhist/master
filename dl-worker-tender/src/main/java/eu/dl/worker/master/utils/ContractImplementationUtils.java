package eu.dl.worker.master.utils;

import eu.dl.dataaccess.dto.generic.Payment;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;

import java.net.URL;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * Utility class used to handle contract implementations.
 */
public final class ContractImplementationUtils {
    /**
     * Default constructor.
     */
    protected ContractImplementationUtils() {
        // utility class
    }

    /**
     * This method updates master tender with data about contract implementations. There are payments
     * mastered including the contract implementations skipped in previous phase. Additionaly, the publication
     * info is added to set of publications.
     *
     * @param tender master tender
     * @param contractImplementations matched tenders - contract implementatiions
     */
    public static void addPaymentsFromContractImplementations(
            final MasterTender tender, final List<MatchedTender> contractImplementations) {
        if (tender == null || tender.getLots() == null || tender.getLots().isEmpty()) {
            return;
        }

        if (contractImplementations != null && !contractImplementations.isEmpty()) {
            for (MatchedTender contractImplementation : contractImplementations) {
                List<MatchedBid> winningBids =  getWinningBids(contractImplementation);
                List<MasterBid> masterWinningBids = getWinningBids(tender);
                if (winningBids != null && !winningBids.isEmpty()
                        && masterWinningBids != null && !masterWinningBids.isEmpty()) {
                    // iterate over bids and try to find corresponding one on the master tender
                    for (MatchedBid bid : winningBids) {
                        for (MasterBid masterBid : masterWinningBids) {
                            if (isSameBidderInBoth(bid.getBidders(), masterBid.getBidders())) {
                                if (masterBid.getPayments() == null || masterBid.getPayments().isEmpty()) {
                                    masterBid.setPayments(bid.getPayments());
                                } else {
                                    masterBid.setPayments(addPayments(
                                            masterBid.getPayments(), bid.getPayments()));
                                }

                                tender.setPublications(addPublications(getIsIncludedPublications(tender),
                                        getIsIncludedPublications(contractImplementation)));

                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Returns all winning bids with payments and bidders.
     *
     * @param tender tender
     *
     * @return list of winning bids with at least one bidder and at least one payment
     */
    private static List<MatchedBid> getWinningBids(final MatchedTender tender) {
        List<MatchedBid> result = new ArrayList<>();
        if (tender != null && tender.getLots() != null) {
            for (MatchedTenderLot lot : tender.getLots()) {
                if (lot.getBids() != null) {
                    for (MatchedBid bid : lot.getBids()) {
                        if (bid.getIsWinning()
                                && bid.getBidders() != null && !bid.getBidders().isEmpty()
                                && bid.getPayments() != null && !bid.getPayments().isEmpty()) {

                            result.add(bid);
                            break;
                        }
                    }
                }
            }
        }

        return result;
    }

    /**
     * Returns all winning bids with bidders.
     *
     * @param tender tender
     *
     * @return list of winning bids with at least one bidder and at least one payment
     */
    private static List<MasterBid> getWinningBids(final MasterTender tender) {
        List<MasterBid> result = new ArrayList<>();
        if (tender != null && tender.getLots() != null) {
            for (MasterTenderLot lot : tender.getLots()) {
                if (lot.getBids() != null) {
                    for (MasterBid bid : lot.getBids()) {
                        if (bid.getIsWinning()
                                && bid.getBidders() != null && !bid.getBidders().isEmpty()) {

                            result.add(bid);
                            break;
                        }
                    }
                }
            }
        }

        return result;
    }

    /**
     * Gets all isIncluded publications.
     *
     * @param matchedTender where to search for publications
     *
     * @return publications found
     */
    private static List<Publication> getIsIncludedPublications(final MatchedTender matchedTender) {
        List<Publication> result = new ArrayList<>();
        if (matchedTender.getPublications() != null) {
            for (Publication publication : matchedTender.getPublications()) {
                if (publication.getIsIncluded()) {
                    result.add(publication);
                }
            }
        }
        return result;
    }


    /**
     * Gets all isIncluded publications.
     *
     * @param masterTender where to search for publications
     *
     * @return publications found
     */
    private static List<Publication> getIsIncludedPublications(final MasterTender masterTender) {
        List<Publication> result = new ArrayList<>();
        if (masterTender.getPublications() != null) {
            for (Publication publication : masterTender.getPublications()) {
                if (publication.getIsIncluded()) {
                    result.add(publication);
                }
            }
        }
        return result;
    }


    /**
     * Checks whether there is the same bidder (the same groupId) in both arrays.
     *
     * @param matchedBodies matched bodies
     * @param masterBodies master bodies
     *
     * @return result of check
     */
    private static Boolean isSameBidderInBoth(final List<MatchedBody> matchedBodies,
            final List<MasterBody> masterBodies) {
        for (MatchedBody matchedBody : matchedBodies) {
            for (MasterBody masterBody : masterBodies) {
                if (matchedBody.getGroupId() != null
                        && matchedBody.getGroupId().equals(masterBody.getGroupId())) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * This method adds new payments to the list of existing ones.
     *
     * @param originalPayments target
     * @param newPayments payments to be added
     *
     * @return merged list
     */
    public static List<Payment> addPayments(final List<Payment> originalPayments,
            final List<Payment> newPayments) {
        if (originalPayments == null) {
            return newPayments;
        }

        if (newPayments == null) {
            return originalPayments;
        }

        for (Payment newPayment : newPayments) {
            Boolean paymentFound = false;
            for (Payment originalPayment : originalPayments) {
                if (compare(originalPayment, newPayment)) {
                    paymentFound = true;
                }
            }

            if (!paymentFound) {
                originalPayments.add(newPayment);
            }
        }

        return originalPayments;
    }

    /**
     * Compares two payments based on date and Price value.
     * @param firstPayment payment
     * @param secondPayment payment
     * @return true if payments are the same ones, false for inequality and null values
     */
    public static Boolean compare(final Payment firstPayment, final Payment secondPayment) {
        if (firstPayment == null || secondPayment == null) {
            return false;
        }

        LocalDate firstDate = firstPayment.getPaymentDate();
        LocalDate secondDate = secondPayment.getPaymentDate();

        if (firstDate != null && secondDate != null) {
            // both dates are non null, lets compare them
            if (firstDate.compareTo(secondDate) != 0) {
                return false;
            }
        } else {
            // dates cannot be compared - includes situation where both of them are
            // not defined, for a such case, payments are not considered to the same ones
            return false;
        }

        Price firstPrice = firstPayment.getPrice();
        Price secondPrice = secondPayment.getPrice();

        if (firstPrice != null && secondPrice != null) {
            // both prices are non null, lets compare them
            if (firstPrice.getCurrency() != null && secondPrice.getCurrency() != null) {
                if (!firstPrice.getCurrency().equals(secondPrice.getCurrency())) {
                    return false;
                }
            } else if ((firstPrice.getCurrency() != null && secondPrice.getCurrency() == null)
                || (firstPrice.getCurrency() == null && secondPrice.getCurrency() != null)) {
                return false;
            }

            if (firstPrice.getNetAmount() != null && secondPrice.getNetAmount() != null) {
                if (!firstPrice.getNetAmount().equals(secondPrice.getNetAmount())) {
                    return false;
                }
            } else if ((firstPrice.getNetAmount() != null && secondPrice.getNetAmount() == null)
                    || (firstPrice.getNetAmount() == null && secondPrice.getNetAmount() != null)) {
                return false;
            } else {
                if (firstPrice.getAmountWithVat() != null && secondPrice.getAmountWithVat() != null) {
                    if (!firstPrice.getAmountWithVat().equals(secondPrice.getAmountWithVat())) {
                        return false;
                    }
                }
            }
        } else {
            // prices cannot be compared - includes situation where both of them are
            // not defined, for a such case, payments are not considered to the same ones
            return false;
        }

        return true;
    }

    /**
     * This method adds new publications to the list of existing ones.
     *
     * @param originalPublications target
     * @param newPublications publications to be added
     *
     * @return merged list
     */
    public static List<Publication> addPublications(final List<Publication> originalPublications,
            final List<Publication> newPublications) {
        if (originalPublications == null) {
            return newPublications;
        }

        if (newPublications == null) {
            return originalPublications;
        }

        for (Publication newPublication : newPublications) {
            Boolean publicationFound = false;
            for (Publication originalPublication : originalPublications) {
                if (compare(originalPublication, newPublication)) {
                    publicationFound = true;
                }
            }

            if (!publicationFound) {
                originalPublications.add(newPublication);
            }
        }

        return originalPublications;
    }

    /**
     * Compares two publications based on publication date, isIncluded and .
     * @param firstPublication publicaton
     * @param secondPublication publication
     * @return true if publications are the same ones, false for inequality and null values
     */
    public static Boolean compare(final Publication firstPublication, final Publication  secondPublication) {
        if (firstPublication == null || secondPublication == null) {
            return false;
        }

        LocalDate firstDate = firstPublication.getPublicationDate();
        LocalDate secondDate = secondPublication.getPublicationDate();

        if (firstDate != null && secondDate != null) {
            // both dates are non null, lets compare them
            if (firstDate.compareTo(secondDate) != 0) {
                return false;
            }
        } else if ((firstDate != null && secondDate == null)
                    || (firstDate == null && secondDate != null)) {

            return false;
        }

        URL firstUrl = firstPublication.getSource();
        URL secondUrl = secondPublication.getSource();

        if (firstUrl != null && secondUrl != null) {
            // both dates are non null, lets compare them
            if (!firstUrl.getPath().equals(secondUrl.getPath())) {
                return false;
            }
        } else if ((firstUrl != null && secondUrl == null)
                || (firstUrl == null && secondUrl != null)) {

            return false;
        }

        Boolean firstIsIncluded = firstPublication.getIsIncluded();
        Boolean secondIsIncluded = secondPublication.getIsIncluded();

        if (firstIsIncluded != null && secondIsIncluded != null) {
            // both dates are non null, lets compare them
            if (firstIsIncluded != secondIsIncluded) {
                return false;
            }
        } else if ((firstIsIncluded != null && secondIsIncluded == null)
                || (firstIsIncluded == null && secondIsIncluded != null)) {

            return false;
        }

        return true;
    }
}
