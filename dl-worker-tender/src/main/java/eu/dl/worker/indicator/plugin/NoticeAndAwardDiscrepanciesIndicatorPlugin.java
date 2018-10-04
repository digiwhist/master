package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.master.MasterTender;
import java.util.Arrays;
import java.util.Map;

import static eu.dl.dataaccess.dto.codetables.PublicationFormType.CONTRACT_NOTICE;
import static eu.dl.dataaccess.dto.codetables.PublicationFormType.CONTRACT_AWARD;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.generic.AwardCriterion;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.dataaccess.utils.TenderUtils;
import eu.dl.worker.utils.ArrayUtils;
import java.util.List;
import java.util.Objects;
import java.util.function.BiPredicate;
import java.util.function.Function;

/**
 * This plugin calculates discrepancies between call for tender and contract award notices.
 */
public class NoticeAndAwardDiscrepanciesIndicatorPlugin extends BaseIndicatorPlugin
    implements IndicatorPlugin<MasterTender> {

    private final MatchedTenderDAO matchedTenderDAO;

    /**
     * Default constructor.
     *
     * @param matchedTenderDAO
     *      matched tender DAO
     */
    public NoticeAndAwardDiscrepanciesIndicatorPlugin(final MatchedTenderDAO matchedTenderDAO) {
        this.matchedTenderDAO = matchedTenderDAO;
    }

    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null) {
            return insufficient();
        }

        Map<PublicationFormType, MatchedTender> latest = TenderUtils.getLatestPublications(tender.getGroupId(),
            matchedTenderDAO, Arrays.asList(CONTRACT_NOTICE, CONTRACT_AWARD));
        
        MatchedTender notice = latest.get(CONTRACT_NOTICE);
        MatchedTender award = latest.get(CONTRACT_AWARD);
        if (notice == null) {
            return insufficient();
        } else if (award == null) {
            return undefined();
        }

        // evaluates indicator
        IndicatorScore score = new IndicatorScore();

        if (notice.getBuyers() != null && notice.getBuyers().size() == 1
            && award.getBuyers() != null && award.getBuyers().size() == 1) {
            Address noticeAddr = notice.getBuyers().get(0).getAddress();
            Address awardAddr = award.getBuyers().get(0).getAddress();
            
            score.test(cmp(noticeAddr, awardAddr, t -> t.getStreet()));
            score.test(cmp(noticeAddr, awardAddr, t -> t.getPostcode()));
        }

        score.test(cmp(notice.getAddressOfImplementation(), award.getAddressOfImplementation(), t -> t.getNuts()));
        score.test(cmp(notice, award, t -> t.getTitle()));
        score.test(cmp(notice, award, t -> t.getIsCoveredByGpa()));
        score.test(cmp(notice, award, t -> t.getSelectionMethod()));
        score.test(cmp(notice, award, t -> t.getIsElectronicAuction()));
        score.test(cmp(notice, award, t -> t.getIsFrameworkAgreement()));
        score.test(cmp(notice, award, t -> t.getIsDps()));


        // AWARD CRITERIA
        BiPredicate<AwardCriterion, AwardCriterion> awardCritCmp =
            (a, b) -> Objects.equals(a.getName(), b.getName()) && Objects.equals(a.getWeight(), b.getWeight());
        // award criteria presented in both pulications (notice and award)
        List<AwardCriterion> critIntersection = ArrayUtils.intersection(notice.getAwardCriteria(),
            award.getAwardCriteria(), awardCritCmp);
        // all presented criteria (by name and weight)
        List<AwardCriterion> critUnion = ArrayUtils.union(notice.getAwardCriteria(), award.getAwardCriteria(),
            awardCritCmp);
        // score update, each criterion which isn't presented in intersection means fail
        critUnion.forEach(n -> {
            score.test(critIntersection.stream().anyMatch(m -> awardCritCmp.test(m, n)));
        });


        // LOTS
        BiPredicate<MatchedTenderLot, MatchedTenderLot> lotCmp =
            (a, b) -> Objects.equals(a.getLotNumber(), b.getLotNumber());
        // lots presented in both publications (notice and award)
        List<MatchedTenderLot> lotsIntersection = ArrayUtils.intersection(notice.getLots(), award.getLots(), lotCmp);
        // all presented lots (by number)
        List<MatchedTenderLot> lotsUnion = ArrayUtils.union(notice.getLots(), award.getLots(), lotCmp);
        // score update, each lot which isn't presented in intersection means fail
        lotsUnion.forEach(n -> {
            score.test(lotsIntersection.stream().anyMatch(m -> lotCmp.test(m, n)));
        });


        // FUNDINGS
        boolean noticeFund = false, awardFund = false;
        // if at least one of fundings is EU funding, consider it as EU funded tender
        if (notice.getFundings() != null) {
            noticeFund = notice.getFundings().stream().anyMatch(n -> Objects.equals(n.getIsEuFund(), Boolean.TRUE));
        }
        if (award.getFundings() != null) {
            awardFund = award.getFundings().stream().anyMatch(n -> Objects.equals(n.getIsEuFund(), Boolean.TRUE));
        }
        score.test(noticeFund == awardFund);

        return score.isInitialized() ? calculated(score.ration()) : insufficient();
    }
    
    /**
     * Applies getter on objects {@code a} and {@code b} and compare returned values.
     *
     * @param <T>
     *      class of underlying object
     * @param a
     *      object a
     * @param b
     *      object b
     * @param getter
     *      getter used for retreiving of the compared values
     * @return TRUE only and only if the both values are same, otherwise FALSE
     */
    private static <T> boolean cmp(final T a, final T b, final Function<T, Object> getter) {
        if (a != b && (a == null || b == null)) {
            return false;
        } else if (a == null && b == null) {
            return true;
        }
        
        return getter.apply(a) == getter.apply(b);
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.ADMINISTRATIVE_NOTICE_AND_AWARD_DISCREPANCIES.name();
    }
}
