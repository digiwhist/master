package eu.digiwhist.worker.cz.clean;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.clean.CleanTenderLot;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.NpwpReason;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.apache.commons.lang.StringUtils;

import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Cleans data from Vestnik Verejnych Zakazek and stores them into database.
 */
public final class VVZTenderCleaner extends BaseVestnikTenderCleaner {
    /**
     * This DateTimeFormatter parses following datetime strings.
     * - d.M.yyyy (time is omitted)
     * - d.M.yyyy H:m
     * Example of date: "17.10.2016 13:00"
     */
    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
            new DateTimeFormatterBuilder().appendPattern("d.M.uuuu[ H:m]")
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .toFormatter());

    private static final List<String> DESIGN_CONTEST_SOURCE_FORM_TYPES = Arrays.asList("F12", "F13");

    @Override
    protected Map<Enum, List<String>> getBodyActivityMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.DEFENCE, Collections.singletonList("DEFENCE"));
        mapping.put(BuyerActivityType.ECONOMIC_AND_FINANCIAL_AFFAIRS,
                Collections.singletonList("ECONOMIC_AND_FINANCIAL_AFFAIRS"));
        mapping.put(BuyerActivityType.EDUCATION, Collections.singletonList("EDUCATION"));
        mapping.put(BuyerActivityType.ENVIRONMENT, Collections.singletonList("ENVIRONMENT"));
        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Collections.singletonList("GENERAL_PUBLIC_SERVICES"));
        mapping.put(BuyerActivityType.HEALTH, Collections.singletonList("HEALTH"));
        mapping.put(BuyerActivityType.HOUSING_AND_COMMUNITY_AMENITIES,
                Collections.singletonList("HOUSING_AND_COMMUNITY_AMENITIES"));
        mapping.put(BuyerActivityType.PUBLIC_ORDER_AND_SAFETY, Collections.singletonList("PUBLIC_ORDER_AND_SAFETY"));
        mapping.put(BuyerActivityType.RECREATION_CULTURE_AND_RELIGION,
                Collections.singletonList("RECREATION_CULTURE_AND_RELIGION"));
        mapping.put(BuyerActivityType.AIRPORT, Collections.singletonList("AIRPORT"));
        mapping.put(BuyerActivityType.ELECTRICITY, Collections.singletonList("ELECTRICITY"));
        mapping.put(BuyerActivityType.COAL_AND_OTHER_EXTRACTION,
                Collections.singletonList("COAL_AND_OTHER_EXTRACTION"));
        mapping.put(BuyerActivityType.GAS_AND_HEAT_PRODUCTION, Collections.singletonList("GAS_AND_HEAT_PRODUCTION"));
        mapping.put(BuyerActivityType.RAILWAY, Collections.singletonList("RAILWAY"));
        mapping.put(BuyerActivityType.URBAN_TRANSPORT, Collections.singletonList("URBAN_TRANSPORT"));
        mapping.put(BuyerActivityType.WATER, Collections.singletonList("WATER"));
        mapping.put(BuyerActivityType.POSTAL, Collections.singletonList("POSTAL"));
        mapping.put(BuyerActivityType.GAS_AND_OIL_EXTRACTION, Collections.singletonList("GAS_AND_OIL_EXTRACTION"));
        mapping.put(BuyerActivityType.PORT, Collections.singletonList("PORT"));
        mapping.put(BuyerActivityType.SOCIAL_PROTECTION, Collections.singletonList("SOCIAL_PROTECTION"));

        return mapping;
    }

    @Override
    protected Map<Enum, List<String>> getSupplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("SERVICES"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("WORKS"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("SUPPLIES"));

        return mapping;
    }

    @Override
    protected Map<Enum, List<String>> getFormTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList("F01", "CZ01"));
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("F02", "F04", "F05", "F12", "CZ02"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("F03", "F06", "F13", "F15", "CZ03"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList("F14", "CZ04"));
        mapping.put(PublicationFormType.CONTRACT_AMENDMENT, Arrays.asList("F20"));
        // map other publications as "other" to avoid throwing exception because the same Levenshtein distance
        mapping.put(PublicationFormType.OTHER, Arrays.asList("F08", "F21", "F24", "CZ05", "CZ06"));

        return mapping;
    }

    @Override
    protected List<DateTimeFormatter> getDateFormatters() {
        return DATETIME_FORMATTERS;
    }

    @Override
    protected Map<Enum, List<String>> getProcedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList("PT_OPEN"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("PT_RESTRICTED"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION,
                Arrays.asList("PT_AWARD_CONTRACT_WITHOUT_CALL", "PT_NEGOTIATED_WITHOUT_PUBLICATION",
                        "PT_NEGOTIATED_WITHOUT_PUBLICATION_24EU"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION,
                Arrays.asList("PT_NEGOTIATED_WITH_PRIOR_CALL", "PT_COMPETITIVE_NEGOTIATION"));
        mapping.put(TenderProcedureType.APPROACHING_BIDDERS, Arrays.asList("PT_SIMPLIFIED_CONTRACT"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("PT_COMPETITIVE_DIALOGUE"));

        return mapping;
    }

    @Override
    protected List<DateTimeFormatter> getDateTimeFormatters() {
        return DATETIME_FORMATTERS;
    }

    @Override
    protected List<String> getDesignContestSourceFormTypes() {
        return DESIGN_CONTEST_SOURCE_FORM_TYPES;
    }

    @Override
    protected Map<Enum, List<String>> getNpwpReasonMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(NpwpReason.NO_VALID_OFFERS_IN_PRECEEDING_PROCUREMENT, Arrays.asList("AnnexD[0].NoTendersRequests"));
        mapping.put(NpwpReason.RESEARCH_PROCUREMENT, Collections.singletonList("AnnexD[0].ManufacturedForResearch"));
        mapping.put(NpwpReason.TECHNICAL_EXCLUSIVITY, Collections.singletonList("D_TECHNICAL"));
        mapping.put(NpwpReason.ART_EXCLUSIVITY, Collections.singletonList("D_ARTISTIC"));
        mapping.put(NpwpReason.AUTHORSHIP_RIGHTS_EXCLUSIVITY, Collections.singletonList("D_PROTECT_RIGHTS"));
        mapping.put(NpwpReason.EMERGENCY, Collections.singletonList("AnnexD[0].ExtremeUrgency"));
        mapping.put(NpwpReason.ADDITIONAL_WORK,
                Arrays.asList("AnnexD[0].DeliveriesOrdered", "AnnexD[0].RepetitionExisting"));
        mapping.put(NpwpReason.PROPOSAL_CONTEST_FOLLOW_UP, Collections.singletonList("AnnexD[0].AwardedDesignContest"));
        mapping.put(NpwpReason.COMMODITY_MARKET, Collections.singletonList("AnnexD[0].CommodityMarket"));
        mapping.put(NpwpReason.ADVANTAGEOUS_CONDITIONS,
                Arrays.asList("AnnexD[0].FromWindingProvider", "AnnexD[0].BarginPurchase"));
        mapping.put(NpwpReason.OUTSIDE_DIRECTIVE, Collections.singletonList("AnnexD[0].OutsideScope"));

        return mapping;
    }

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }

    @Override
    protected CleanTender postProcessVestnikOrVvzSpecificRules(final ParsedTender parsedTender,
            final CleanTender cleanTender) {

        Publication includedPublication = getIncludedPublication(cleanTender);
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
                cleanTender.setLots(matchAndMergeLotsWithinOneForm(cleanTender.getLots()));
            }
        }

        return cleanTender;
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

        // one default part
        if (unmatchedLots.size() == 2) {
            CleanTenderLot lot1 = unmatchedLots.get(0);
            CleanTenderLot lot2 = unmatchedLots.get(1);
            if (lot1.getLotNumber() == null && lot2.getLotNumber() == null && lot1.getPositionOnPage()
                    == lot2.getPositionOnPage()) {
                return Collections.singletonList(mergeLots(lot1, lot2));
            }
        } else if (unmatchedLots.size() > 2) {
            // result will be saved in mergedLots list
            List<CleanTenderLot> mergedLots = new ArrayList<>();
            // first add all the lots that do not have lotNumber (these cannot be merged, so they go straight to the
            // result list
            mergedLots.addAll(
                    unmatchedLots.stream().filter(lot -> lot.getLotNumber() == null).collect(Collectors.toList()));
            // remove these lots without lotNumber from input list of lots
            unmatchedLots.removeAll(mergedLots);

            // for the rest of the input lots, try to find those with same lotNumber, if they are in pair, merge
            // them into one lot, otherwise add to result list without merging
            List<CleanTenderLot> examinedLots = new ArrayList<>();
            for (CleanTenderLot lot : unmatchedLots) {
                if (examinedLots.contains(lot)) {
                    continue;
                }
                List<CleanTenderLot> lotsWithSameNumber = unmatchedLots.stream()
                        .filter(l -> !l.equals(lot) && l.getLotNumber() == lot.getLotNumber())
                        .collect(Collectors.toList());
                if (lotsWithSameNumber.size() == 1) {
                    CleanTenderLot lotWithSameNumber = lotsWithSameNumber.get(0);
                    mergedLots.add(mergeLots(lot, lotWithSameNumber));
                } else {
                    mergedLots.add(lot);
                    mergedLots.addAll(lotsWithSameNumber);
                }
                examinedLots.addAll(lotsWithSameNumber);
            }
            return mergedLots;
        }

        return unmatchedLots;
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
    private CleanTenderLot mergeLots(final CleanTenderLot lot1, final CleanTenderLot lot2) {
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
                .setLotId(lotWithOzInfo.getLotId());

        if (StringUtils.isNotEmpty(lotWithOzInfo.getTitle())) {
            lotWithOzzInfo.setTitle(lotWithOzInfo.getTitle());
        }

        return lotWithOzzInfo;
    }
}
