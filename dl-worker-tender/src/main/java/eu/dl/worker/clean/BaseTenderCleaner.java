package eu.dl.worker.clean;

import eu.dl.dataaccess.dto.clean.BaseCleanTenderLot;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.clean.CleanTenderLot;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.generic.AwardCriterion;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.utils.DTOUtils;
import eu.dl.worker.clean.plugin.BooleanPlugin;
import eu.dl.worker.clean.plugin.CpvPlugin;
import eu.dl.worker.clean.plugin.LongTextPlugin;
import eu.dl.worker.clean.plugin.ShortTextPlugin;
import eu.dl.worker.clean.plugin.URLPlugin;
import org.apache.commons.lang3.BooleanUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * This class covers the main functionality for the cleaners implementation.
 *
 */
public abstract class BaseTenderCleaner extends BaseCleaner<ParsedTender, CleanTender> {

    /**
     * Default constructor.
     */
    protected BaseTenderCleaner() {
        super();
    }

    @Override
    protected final void registerCommonPlugins() {
        pluginRegistry
                .registerPlugin("shortText", new ShortTextPlugin())
                .registerPlugin("longText", new LongTextPlugin())
                .registerPlugin("boolean", new BooleanPlugin())
                .registerPlugin("cpvs", new CpvPlugin())
                .registerPlugin("urls", new URLPlugin());
        logger.debug("Registered common plugins to registry.");
    }

    @Override
    protected final CleanTender postProcessCommonRules(final CleanTender cleanTender, final ParsedTender parsedTender) {
        updateAwardCriteria(cleanTender);
        if (cleanTender.getLots() != null) {
            for (CleanTenderLot lot : cleanTender.getLots()) {
                updateAwardCriteria(lot);
            }
        }

        updateLotStatus(cleanTender);

        updateLotCancellationDate(cleanTender);

        mergeFrameworkAgreementLots(cleanTender);

        cleanTender.setCountry(parsedTender.getCountry());

        processContractImplementations(cleanTender);

        return cleanTender;
    }

    /**
     * This method updates lot statuses based on publication form type.
     *
     * @param cleanTender lots are updated for this tender
     */
    private void updateLotStatus(final CleanTender cleanTender) {
        if (cleanTender == null || cleanTender.getLots() == null) {
            return;
        }

        // lot status update for whole cancelled tender
        if (Boolean.TRUE.equals(cleanTender.getIsWholeTenderCancelled())) {
            cleanTender.getLots().forEach(l -> l.setStatus(TenderLotStatus.CANCELLED));
        } else {
            for (CleanTenderLot lot : cleanTender.getLots()) {
                if (lot.getStatus() == null) {
                    if (lot.getCancellationDate() != null || lot.getCancellationReason() != null) {
                        lot.setStatus(TenderLotStatus.CANCELLED);
                    } else {
                        lot.setStatus(TenderLotStatus.fromPublicationFormType(DTOUtils.getPublicationFormType(cleanTender)));
                    }
                }
            }
        }
    }

    /**
     * Method sets correct award criteria when selection method is LOWEST PRICE.
     *
     * @param <C>
     *     clean entity
     * @param baseCleanTenderLot
     *     clean tender/lot
     */
    private <C> void updateAwardCriteria(final BaseCleanTenderLot<C> baseCleanTenderLot) {
        // create a 100% price related criterion when selection method is LOWEST PRICE
        if (baseCleanTenderLot.getSelectionMethod() == SelectionMethod.LOWEST_PRICE) {
            if (baseCleanTenderLot.getAwardCriteria() != null) {
                boolean tenderHasOneAwardCriterionAndIsIncorrect = baseCleanTenderLot.getAwardCriteria().size() == 1
                        && ((baseCleanTenderLot.getAwardCriteria().get(0).getWeight() != null
                        && baseCleanTenderLot.getAwardCriteria().get(0).getWeight() != 100)
                        || (baseCleanTenderLot.getAwardCriteria().get(0).getIsPriceRelated() != null
                        && !baseCleanTenderLot.getAwardCriteria().get(0).getIsPriceRelated()));
                boolean tenderHasMoreAwardCriteria = baseCleanTenderLot.getAwardCriteria().size() > 1;
                if (tenderHasOneAwardCriterionAndIsIncorrect || tenderHasMoreAwardCriteria) {
                    logger.warn("Selection method is LOWEST PRICE and parsed award criterion is incorrectly set.");
                }
            }
            
            baseCleanTenderLot.setAwardCriteria(new ArrayList(Collections.singletonList(new AwardCriterion()
                .setName("PRICE")
                .setWeight(100)
                .setIsPriceRelated(true))));
        }
    }

    /**
     * Framework agreements usually don't have lots, so if there are lots, it's most likely just more winners.
     * Tries to find such cases (which are incorrectly parsed as multiple lots) and saves all the winners (winning
     * bids) within one lot.
     *
     * @param cleanTender
     *  clean tender
     */
    private void mergeFrameworkAgreementLots(final CleanTender cleanTender) {
        Publication includedPublication = getIncludedPublication(cleanTender);
        PublicationFormType formType = null;
        if (includedPublication != null) {
            formType = includedPublication.getFormType();
        }

        // if tender
        // a) has multiple lots and
        // b) is framework agreement and
        // c) is CONTRACT_AWARD or CONTRACT_IMPLEMENTATION (included publication)
        // d) has same bidsCount on all lots and same title on all lots
        // then move all bids under first lot (yes, there will be more winning bids per lot), delete other lots.

        List<CleanTenderLot> lots = cleanTender.getLots();
        // a) if tender has multiple lots
        if (lots != null && lots.size() > 1) {
            CleanTenderLot firstLot = lots.get(0);
            // b) and is framework agreement or dps
            if (BooleanUtils.isTrue(cleanTender.getIsFrameworkAgreement())) {
                // c) and is CONTRACT_AWARD or CONTRACT_IMPLEMENTATION
                if (formType == PublicationFormType.CONTRACT_AWARD || formType == PublicationFormType.CONTRACT_IMPLEMENTATION) {
                    // d) and has same bidsCount or title on all lots
                    if (lots.stream().allMatch(lot ->
                        Objects.equals(lot.getBidsCount(), firstLot.getBidsCount()) && Objects.equals(lot.getTitle(), firstLot.getTitle()))
                    ) {
                        // move all the bids under the first lot
                        firstLot.setBids(lots.stream().
                            map(CleanTenderLot::getBids).filter(Objects::nonNull).flatMap(List::stream)
                            .collect(Collectors.toList()));
                        // delete other lots
                        cleanTender.setLots(new ArrayList<>(Collections.singletonList(firstLot)));
                    }
                }
            }
        }
    }

    /**
     * This method is used to clean contract implementations. there are two phases:
     * 1) Form_type is changed to CONTRACT_IMPLEMENTATION for certain publications (isIncluded = true,
     * formType = CONTRACT_AWARD and tender.procedureType = MINITENDER)
     * 2) for publication[isIncluded=TRUE].formType = CONTRACT_IMPLEMENTATION we generate payments.
     *
     * @param cleanTender tender to be modified
     */
    private void processContractImplementations(final CleanTender cleanTender) {
        if (cleanTender.getPublications() != null) {
            // change publications to contract implementation where needed
            for (Publication publication : cleanTender.getPublications()) {
                if (Boolean.TRUE.equals(publication.getIsIncluded())
                        && publication.getFormType() == PublicationFormType.CONTRACT_AWARD
                        && cleanTender.getProcedureType() == TenderProcedureType.MINITENDER) {
                    // change the publication type
                    publication.setFormType(PublicationFormType.CONTRACT_IMPLEMENTATION);
                }
            }
        }
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
        if (cleanTender.getPublications() == null) {
            return null;
        }
        
        return cleanTender.getPublications()
                .stream()
                .filter(p -> Boolean.TRUE.equals(p.getIsIncluded()))
                .findFirst()
                .orElse(null);
    }

    /**
     * This method updates cancellation date of cancelled lots.
     *
     * @param cleanTender lots are updated for this tender
     */
    private void updateLotCancellationDate(final CleanTender cleanTender) {
        if (cleanTender == null || cleanTender.getLots() == null) {
            return;
        }

        Publication publication = getIncludedPublication(cleanTender);
        if (publication != null && cleanTender.getLots() != null) {
            cleanTender.getLots().stream()
                .filter(n -> n.getStatus() == TenderLotStatus.CANCELLED && n.getCancellationDate() == null)
                .forEach(n -> n.setCancellationDate(publication.getPublicationDate()));
        }
    }
}
