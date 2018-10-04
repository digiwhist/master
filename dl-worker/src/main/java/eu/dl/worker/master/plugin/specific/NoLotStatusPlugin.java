package eu.dl.worker.master.plugin.specific;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.utils.BasePlugin;

import java.util.Arrays;
import java.util.List;

import static eu.dl.dataaccess.dto.codetables.PublicationFormType.CONTRACT_AWARD;
import static eu.dl.dataaccess.dto.codetables.PublicationFormType.CONTRACT_CANCELLATION;
import static eu.dl.dataaccess.dto.codetables.PublicationFormType.PRIOR_INFORMATION_NOTICE;
import static eu.dl.dataaccess.dto.codetables.PublicationFormType.CONTRACT_NOTICE;

/**
 * This plugin is used only if the master tender includes no lots. Creates lot with set status field depending on latest publications.
 */
public class NoLotStatusPlugin
        extends BasePlugin implements MasterPlugin<MatchedTender, MasterTender, MatchedTender> {

    @Override
    public final MasterTender master(final List<MatchedTender> matched, final MasterTender finalItem,
                                     final List<MatchedTender> context) {
        if (finalItem.getLots() != null && !finalItem.getLots().isEmpty()) {
            return finalItem;
        }

        if (Boolean.TRUE.equals(finalItem.getIsWholeTenderCancelled())) {
            return finalItem.addLot(new MasterTenderLot().setStatus(TenderLotStatus.CANCELLED));
        }

        Publication latest = getLatestPublished(finalItem.getPublications(), CONTRACT_AWARD, CONTRACT_CANCELLATION);
        if (latest == null) {
            latest = getLatestPublished(finalItem.getPublications(), CONTRACT_NOTICE, PRIOR_INFORMATION_NOTICE);
        }

        if (latest == null) {
            return finalItem;
        }

        final TenderLotStatus status;
        switch (latest.getFormType()) {
            case CONTRACT_AWARD:
                status = TenderLotStatus.AWARDED;
                break;
            case CONTRACT_NOTICE:
                status = TenderLotStatus.ANNOUNCED;
                break;
            case CONTRACT_CANCELLATION:
                status = TenderLotStatus.CANCELLED;
                break;
            case PRIOR_INFORMATION_NOTICE:
                status = TenderLotStatus.PREPARED;
                break;
            default:
                return finalItem;
        }

        finalItem.addLot(new MasterTenderLot().setStatus(status));

        return finalItem;
    }

    /**
     * @param publications
     *      list of publications to be searched
     * @param type
     *      form type(s) to be found
     * @return last published publication of the given type or null. In case that the publications have same date of publication,
     *      the first found wins.
     */
    private static Publication getLatestPublished(final List<Publication> publications, final PublicationFormType... type) {
        if (publications == null) {
            return null;
        }

        return publications.stream()
            .filter(n -> n != null && Boolean.TRUE.equals(n.getIsIncluded()) && n.getFormType() != null
                && Arrays.asList(type).contains(n.getFormType()))
            .reduce(null, (m, n) -> {
                if (m == null || (m.getPublicationDate() == null && n.getPublicationDate() != null)) {
                    return n;
                } else if (n.getPublicationDate() == null) {
                    return m;
                }

                return n.getPublicationDate().isAfter(m.getPublicationDate()) ? n : m;
            });
    }
}
