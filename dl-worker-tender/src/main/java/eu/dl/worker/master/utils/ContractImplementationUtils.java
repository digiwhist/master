package eu.dl.worker.master.utils;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.generic.Payment;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import org.apache.commons.lang3.ObjectUtils;

import java.net.URL;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

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
     * mastered including the contract implementations skipped in previous phase. Additionally, the publication
     * info is added to set of publications.
     *
     * @param tender
     *      master tender
     * @param implementations
     *      matched tenders - contract implementations
     */
    public static void addPaymentsFromContractImplementations(final MasterTender tender, final List<MatchedTender> implementations) {
        if (tender == null || tender.getLots() == null || tender.getLots().isEmpty()
                || implementations == null || implementations.isEmpty()) {
            return;
        }

        List<MasterBid> masterWinningBids = getWinningBids(tender);
        if (masterWinningBids.isEmpty()) {
            return;
        }

        List<Publication> newPublications = new ArrayList<>();

        implementations.stream()
            .filter(t -> t.getLots() != null)
            .forEach(t -> {
                Publication contractImplementation = getIncludedPublication(t, PublicationFormType.CONTRACT_IMPLEMENTATION);

                LocalDate publicationDate = contractImplementation.getPublicationDate();

                t.getLots().stream()
                    .filter(l -> l.getBids() != null)
                    .forEach(l -> {
                        LocalDate paymentDate = ObjectUtils.firstNonNull(l.getContractSignatureDate(), l.getAwardDecisionDate(),
                            t.getContractSignatureDate(), t.getAwardDecisionDate(), publicationDate);

                        String impMainCpv = getMainCpvCode(l.getCpvs());

                        l.getBids().stream()
                            .filter(b -> Boolean.TRUE.equals(b.getIsWinning()))
                            .forEach(b -> {
                                Price paymentPrice = b.getPrice() != null ? b.getPrice() : t.getFinalPrice();
                                Payment payment = new Payment().setPrice(paymentPrice).setPaymentDate(paymentDate);

                                // search by bidders
                                for (MasterBid masterBid : masterWinningBids) {
                                    if (isSameBidderInBoth(b.getBidders(), masterBid.getBidders())) {
                                        masterBid.setPayments(addPayment(masterBid.getPayments(), payment));
                                        break;
                                    }
                                }
                            });
                    });

                newPublications.add(contractImplementation);
            });

        if (!newPublications.isEmpty()) {
            // contract implementation urls
            List<URL> urls = newPublications.stream()
                .filter(p -> Boolean.TRUE.equals(p.getIsIncluded()))
                .map(p -> ObjectUtils.firstNonNull(p.getHumanReadableUrl(), p.getMachineReadableUrl()))
                .collect(Collectors.toList());

            // remove non-included CONTRACT_AWARD with same human(machine)ReadableUrl
            List<Publication> finalPublications = tender.getPublications().stream()
                .filter(p -> !(Boolean.FALSE.equals(p.getIsIncluded()) && p.getFormType() == PublicationFormType.CONTRACT_AWARD
                    && ObjectUtils.anyNotNull(p.getHumanReadableUrl(), p.getMachineReadableUrl())
                    && urls.contains(ObjectUtils.firstNonNull(p.getHumanReadableUrl(), p.getMachineReadableUrl()))))
                .collect(Collectors.toList());

            finalPublications = addPublications(finalPublications, newPublications);

            tender.setPublications(finalPublications);
        }
    }

    /**
     * @param tender
     *      master tender. It should contain the lots.
     * @param lotGetter
     *      the function that returns the value of lot which is considered in comparison
     * @param value
     *      the value which is compared with lot's value
     * @return the first suitable lot or NULL.
     */
    private static MasterTenderLot findLotBy(final MasterTender tender, final Function<MasterTenderLot, String> lotGetter,
                                             final String value) {
        if (tender == null || tender.getLots() == null) {
            return null;
        }

        for (MasterTenderLot l : tender.getLots()) {
            String lotValue = lotGetter.apply(l);
            if (ObjectUtils.allNotNull(lotValue, value) && Objects.equals(lotValue, value)) {
                return l;
            }
        }

        return null;
    }

    /**
     * @param cpvs
     *      list of cpvs
     * @return first non-null main cpv code or NULL
     */
    private static String getMainCpvCode(final List<CPV> cpvs) {
        if (cpvs == null || cpvs.isEmpty()) {
            return null;
        }

        return cpvs.stream()
            .filter(n -> Boolean.TRUE.equals(n.getIsMain()))
            .map(CPV::getCode).filter(Objects::nonNull).findFirst().orElse(null);
    }

    /**
     * Returns all winning bids with bidders.
     *
     * @param tender
     *      tender
     * @return list of winning bids with at least one bidder and at least one payment
     */
    private static List<MasterBid> getWinningBids(final MasterTender tender) {
        List<MasterBid> result = new ArrayList<>();
        if (tender != null && tender.getLots() != null) {
            for (MasterTenderLot lot : tender.getLots()) {
                if (lot.getBids() != null) {
                    for (MasterBid bid : lot.getBids()) {
                        if (bid.getIsWinning() && bid.getBidders() != null && !bid.getBidders().isEmpty()) {
                            result.add(bid);
                        }
                    }
                }
            }
        }

        return result;
    }

    /**
     * Checks whether there is the same bidder (the same groupId) in both arrays.
     *
     * @param matchedBodies
     *      matched bodies
     * @param masterBodies
     *      master bodies
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
     * @param originalPayments
     *      target
     * @param newPayments
     *      payments to be added
     * @return merged list
     */
    public static List<Payment> addPayments(final List<Payment> originalPayments, final List<Payment> newPayments) {
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
     * This method adds new payment to the list of existing ones.
     *
     * @param originalPayments
     *      target
     * @param newPayment
     *      payment to be added
     * @return merged list
     */
    public static List<Payment> addPayment(final List<Payment> originalPayments, final Payment newPayment) {
        List<Payment> newPayments = new ArrayList<>();
        newPayments.add(newPayment);
        return addPayments(originalPayments, newPayments);
    }

    /**
     * Compares two payments based on date and Price value.
     *
     * @param firstPayment
     *      payment
     * @param secondPayment
     *      payment
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
     * @param originalPublications
     *      target
     * @param newPublications
     *      publications to be added
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
     * Gets first included publication of given form type.
     *
     * @param matchedTender
     *      where to search for publications
     * @param formType
     *      form type
     * @return publications found
     */
    private static Publication getIncludedPublication(final MatchedTender matchedTender, final PublicationFormType formType) {
        if (matchedTender.getPublications() != null) {
            return matchedTender.getPublications().stream()
                    .filter(p -> Boolean.TRUE.equals(p.getIsIncluded()) && formType == p.getFormType())
                    .findFirst().orElse(null);
        }
        return null;
    }

    /**
     * Compares two publications.
     *
     * @param firstPublication
     *      publication
     * @param secondPublication
     *      publication
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
