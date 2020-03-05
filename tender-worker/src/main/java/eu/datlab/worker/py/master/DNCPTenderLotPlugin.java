package eu.datlab.worker.py.master;

import eu.datlab.worker.master.plugin.BaseDatlabTenderLotPlugin;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;

import java.util.List;
import java.util.Objects;

/**
 * Paraguay tender lot mastering plugin.
 */
public final class DNCPTenderLotPlugin extends BaseDatlabTenderLotPlugin {

    /**
     * @param firstLot
     *          first matched lot
     * @param secondLot
     *          second matched lot
     * @param items
     *          list of matched lot lists (each matched lot list represents lots of one matched tender)
     * @return returns 1 if the passed lots have equal contractNumberId, otherwise 0
     */
    @Override
    protected Double calculateMatchingRatio(final MatchedTenderLot firstLot, final MatchedTenderLot secondLot,
                                            final List<List<MatchedTenderLot>> items) {

        return Objects.equals(firstLot.getContractNumber(), secondLot.getContractNumber()) ? 1.0 : 0.0;
    }
}
