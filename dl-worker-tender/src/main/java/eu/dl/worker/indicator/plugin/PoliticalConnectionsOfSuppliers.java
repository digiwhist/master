package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;

import static eu.dl.dataaccess.dto.codetables.BodyIdentifier.Type.BVD_ID;

/**
 * Political connections indicator.
 */
public final class PoliticalConnectionsOfSuppliers extends BaseIndicatorPlugin implements
        IndicatorPlugin<MasterTender> {

    private MasterBodyDAO masterBodyDAO;

    /**
     * Default constructor.
     *
     * @param masterBodyDAO dao to search with for bvd id.
     */
    public PoliticalConnectionsOfSuppliers(final MasterBodyDAO masterBodyDAO) {
        this.masterBodyDAO = masterBodyDAO;
    }

    @Override
    public Indicator evaluate(final MasterTender item) {

        if (item.getPublications().stream().noneMatch(publication -> publication.getFormType() == PublicationFormType
                .CONTRACT_AWARD)) {
            return undefined();
        } else if (item.getLots() == null
                || item.getLots().stream().allMatch(lot -> lot.getBids() == null
                || lot.getBids().stream().filter(bid -> bid.getBidders() != null).allMatch(bid -> bid
                .getBidders() == null
                || bid.getBidders().stream().filter(bidder -> bidder.getBodyIds() != null).allMatch(bidder -> bidder
                .getBodyIds() == null)))) {
            return insufficient();
        } else if (item.getLots().stream().filter(lot -> lot.getBids() != null)
                    .allMatch(lot -> lot.getBids().stream().filter(bid -> bid.getBidders() != null)
                    .allMatch(bid -> bid.getBidders().stream().filter(bidder -> bidder.getBodyIds() != null)
                    .allMatch(bidder -> bidder.getBodyIds().stream()
                    .allMatch(id -> id.getType() == BVD_ID && id.getId() != null && isBidWinning(bid)
                            && masterBodyDAO.existsInPoliticalExposedPersons(id.getId())))))) {
            return calculated(0d);
        } else if (item.getLots().stream().filter(lot -> lot.getBids() != null)
                    .anyMatch(lot -> lot.getBids().stream().filter(bid -> bid.getBidders() != null)
                    .anyMatch(bid -> bid.getBidders().stream().filter(bidder -> bidder.getBodyIds() != null)
                    .anyMatch(bidder -> bidder.getBodyIds().stream()
                    .anyMatch(id -> id.getType() == BVD_ID && id.getId() != null && isBidWinning(bid)
                            && masterBodyDAO.existsInPoliticalExposedPersons(id.getId())))))) {
            return calculated(100d);
        } else {
            return insufficient();
        }
    }

    @Override
    public String getType() {
        return TenderIndicatorType.TRANSPARENCY_NUMBER_OF_KEY_MISSING_FIELDS.name();
    }

    /**
     * Check that bid isWinning is not null and is winning is true.
     *
     * @param bid bid to be checked
     * @return boolean
     */
    private boolean isBidWinning(final MasterBid bid) {
        return bid.getIsWinning() != null && bid.getIsWinning();
    }
}
