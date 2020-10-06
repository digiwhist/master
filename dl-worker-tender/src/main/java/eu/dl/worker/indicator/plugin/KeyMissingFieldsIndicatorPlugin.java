package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dao.CleanTenderDAO;
import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.clean.CleanBid;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.clean.CleanTenderLot;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.utils.TenderUtils;

import java.util.Arrays;
import java.util.Map;
import java.util.Objects;

import static eu.dl.dataaccess.dto.codetables.PublicationFormType.CONTRACT_AWARD;
import static eu.dl.dataaccess.dto.codetables.PublicationFormType.CONTRACT_NOTICE;

/**
 * This plugin calculates number of key missing fields in form.
 */
public class KeyMissingFieldsIndicatorPlugin extends BaseIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    private final MatchedTenderDAO matchedTenderDAO;

    private final CleanTenderDAO cleanTenderDAO;

    /**
     * Default constructor.
     *
     * @param matchedTenderDAO
     *      matched tender DAO
     * @param cleanTenderDAO
     *      clean tender DAO
     */
    public KeyMissingFieldsIndicatorPlugin(final MatchedTenderDAO matchedTenderDAO,
        final CleanTenderDAO cleanTenderDAO) {
        this.matchedTenderDAO = matchedTenderDAO;
        this.cleanTenderDAO = cleanTenderDAO;
    }

    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null) {
            return insufficient();
        }
        
        Map<PublicationFormType, MatchedTender> latest = TenderUtils.getLatestPublications(tender.getGroupId(),
            matchedTenderDAO, Arrays.asList(CONTRACT_NOTICE, CONTRACT_AWARD));

        // evaluates indicator
        IndicatorScore score = new IndicatorScore();

        latest.entrySet().forEach(n -> {
            CleanTender cleanTender = (CleanTender) cleanTenderDAO.getById(n.getValue().getCleanObjectId());

            if (n.getKey() == CONTRACT_NOTICE) {
                evaluateContractNotice(cleanTender, score);
            } else if (n.getKey() == CONTRACT_AWARD) {
                evaluateContractAward(cleanTender, score);
            }
        });

        return score.isInitialized() ? calculated(score.ration()) : insufficient();
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.TRANSPARENCY_NUMBER_OF_KEY_MISSING_FIELDS.name();
    }

    /**
     * Evaluates score for contract notice.
     *
     * @param tender
     *      clean tender, should be of CONTRACT_NOTICE type
     * @param score
     *      current score
     * @return updated score
     */
    private static IndicatorScore evaluateContractNotice(final CleanTender tender, final IndicatorScore score) {
        if (tender.getAwardCriteria() != null && tender.getSelectionMethod() == SelectionMethod.MEAT) {
            tender.getAwardCriteria().forEach(n -> {
                score.test(n.getName() != null);
                score.test(n.getWeight() != null);
            });
        }

        if (tender.getLots() != null) {
            tender.getLots().forEach(n -> {
                score.test(Arrays.asList(n.getEstimatedStartDate(), n.getEstimatedCompletionDate(),
                        n.getEstimatedDurationInMonths(), n.getEstimatedDurationInDays()).stream()
                    .anyMatch(m -> m != null));
                score.test(n.getSelectionMethod() != null);
            });
        }

        score.test(tender.getEligibleBidLanguages() != null);
        score.test(tender.getSelectionMethod() != null);

        if (tender.getCpvs() != null) {
            tender.getCpvs().forEach(n -> score.test(n.getCode() != null));
        }

        return score;
    }

    /**
     * Evaluates score for contract award.
     *
     * @param tender
     *      clean tender, should be of CONTRACT_AWARD type
     * @param score
     *      current score
     * @return updated score
     */
    private static IndicatorScore evaluateContractAward(final CleanTender tender, final IndicatorScore score) {
        if (tender.getAddressOfImplementation() != null && tender.getAddressOfImplementation().getNuts() != null) {
            tender.getAddressOfImplementation().getNuts().forEach(n -> score.test(n != null));
        }

        if (tender.getLots() != null) {
            for (CleanTenderLot lot : tender.getLots()) {
                if (lot.getBids() != null) {
                    for (CleanBid bid : lot.getBids()) {
                        if (Objects.equals(bid.getIsWinning(), Boolean.TRUE)) {
                            if (bid.getBidders() != null) {
                                bid.getBidders().forEach(n -> score.test(n.getName() != null));
                            }

                            if (bid.getPrice() != null) {
                                score.test(bid.getPrice().getNetAmount() != null);
                            }
                            
                            score.test(bid.getIsSubcontracted() != null);
                        }
                    }
                }
            }
        }

        if (tender.getFundings() != null) {
            tender.getFundings().forEach(f -> score.test(f.getIsEuFund() != null));
        }

        return score;
    }
}
