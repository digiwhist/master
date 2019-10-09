package eu.datlab.worker.cz.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.clean.CleanTenderLot;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BaseCleaningPlugin;
import org.apache.commons.lang.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiPredicate;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * Plugin merges lots.
 */
public final class VVZLotsMatchingPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {
    @Override
    public CleanTender clean(final ParsedTender parsedItem, final CleanTender cleanItem) {
        Publication includedPublication = getIncludedPublication(cleanItem);
        PublicationFormType formType = null;
        if (includedPublication != null) {
            formType = includedPublication.getFormType();
        }

        // match lots for awards (award forms have some info about lots in section II and some info in section V
        // which leads to duplicate lots in parsing
        // only process contract awards
        if (formType != null && formType.equals(PublicationFormType.CONTRACT_AWARD)) {
            String sourceFormType = includedPublication.getSourceFormType();
            // only contract awards of european forms (above-the-threshold tenders)
            if (sourceFormType != null && sourceFormType.startsWith("F")) {
                cleanItem.setLots(matchAndMergeLotsWithinOneForm(cleanItem.getLots()));
            }
        }

        return cleanItem;
    }

    /**
     * Returns publication that has isIncluded set to true.
     *
     * @param cleanTender
     *         clean tender
     *
     * @return included (main) publication or null if none exists
     */
    private Publication getIncludedPublication(final CleanTender cleanTender) {
        return cleanTender.getPublications()
            .stream()
            .filter(p -> p.getIsIncluded() != null && p.getIsIncluded())
            .findFirst()
            .orElse(null);
    }

    /**
     * Tries to find pairs of lots within the form and merge them.
     *
     * @param unmatchedLots
     *         clean tender lots
     *
     * @return list of clean tender lots where pairs of same lots are merged
     */
    private List<CleanTenderLot> matchAndMergeLotsWithinOneForm(final List<CleanTenderLot> unmatchedLots) {
        if (unmatchedLots == null) {
            return null;
        }

        // result of merging
        List<CleanTenderLot> result = new ArrayList<>();

        // lots matching by lot number
        if (unmatchedLots.size() > 2) {
            merge(l -> l.getLotNumber() != null, (l1, l2) -> l1.getLotNumber() == l2.getLotNumber(), unmatchedLots, result);
        }

        // lots matching by position on page
        if (unmatchedLots.size() >= 2) {
            merge(l -> l.getPositionOnPage() != null, (l1, l2) -> l1.getPositionOnPage() == l2.getPositionOnPage(), unmatchedLots, result);
        }

        // add all unmatched lots to the result
        result.addAll(unmatchedLots);

        return result;
    }

    /**
     * Updates {@code result} list with successfully merged lots from {@code unmatched} list. Merged lots are removed from the list of
     * unmatched lots.
     *
     * @param filter
     *      filter which returns lots for matching from {@code unmatched} list
     * @param matcher
     *      predicate which returns TRUE for the two same lots
     * @param unmatched
     *      list of unmatched lots
     * @param result
     *      list of merged lots
     */
    private static void merge(final Predicate<CleanTenderLot> filter, final BiPredicate<CleanTenderLot, CleanTenderLot> matcher,
                              final List<CleanTenderLot> unmatched, final List<CleanTenderLot> result) {
        // get lots for matching process
        List<CleanTenderLot> lots = unmatched.stream().filter(filter).collect(Collectors.toList());

        List<CleanTenderLot> examined = new ArrayList<>();
        for (CleanTenderLot lot : lots) {
            if (examined.contains(lot)) {
                continue;
            }

            // attempt to find matches with current lot, if they are in a pair, merge them into one lot and add to result
            List<CleanTenderLot> matched = lots.stream()
                .filter(l -> matcher.test(l, lot)).collect(Collectors.toList());

            if (matched.size() == 2) {
                CleanTenderLot m1 = matched.get(0);
                CleanTenderLot m2 = matched.get(1);

                result.add(mergeLots(m1, m2));
                unmatched.remove(m1);
                unmatched.remove(m2);
            }
            examined.addAll(matched);
        }
    }

    /**
     * Merges two lots together (one is from section II, one is from section V).
     *
     * @param lot1
     *         first lot
     * @param lot2
     *         second lot
     *
     * @return merged lot from lot1 and lot2
     */
    private static CleanTenderLot mergeLots(final CleanTenderLot lot1, final CleanTenderLot lot2) {
        CleanTenderLot lotWithOzInfo = lot1;
        CleanTenderLot lotWithOzzInfo = lot2;
        if (lot1.getIsAwarded() != null) {
            lotWithOzInfo = lot2;
            lotWithOzzInfo = lot1;
        }

        lotWithOzzInfo.setPositionOnPage(lotWithOzInfo.getPositionOnPage())
            .setCpvs(lotWithOzInfo.getCpvs())
            .setAddressOfImplementation(lotWithOzInfo.getAddressOfImplementation())
            .setDescription(lotWithOzInfo.getDescription())
            .setSelectionMethod(lotWithOzInfo.getSelectionMethod())
            .setAwardCriteria(lotWithOzInfo.getAwardCriteria())
            .setHasOptions(lotWithOzInfo.getHasOptions())
            .setFundings(lotWithOzInfo.getFundings())
            .setLotId(lotWithOzInfo.getLotId())
            .setLotNumber(lotWithOzInfo.getLotNumber());

        if (StringUtils.isNotEmpty(lotWithOzInfo.getTitle())) {
            lotWithOzzInfo.setTitle(lotWithOzInfo.getTitle());
        }

        return lotWithOzzInfo;
    }
}
