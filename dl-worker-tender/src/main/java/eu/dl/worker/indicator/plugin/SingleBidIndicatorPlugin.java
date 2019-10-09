package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;

/**
 * This plugin calculates Single bid indicator.
 */
public class SingleBidIndicatorPlugin extends LotAverageIndicatorPlugin {
    /**
     * Constructor sets lot level indicator plugin.
     */
    public SingleBidIndicatorPlugin() {
        super(new LotSingleBidIndicatorPlugin());
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.INTEGRITY_SINGLE_BID.name();
    }
}
