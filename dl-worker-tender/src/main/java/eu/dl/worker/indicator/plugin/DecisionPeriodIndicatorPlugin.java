package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;

/**
 * This plugin calculates decision period length.
 */
public class DecisionPeriodIndicatorPlugin extends LotAverageIndicatorPlugin {
    /**
     * Constructor sets lot level indicator plugin.
     */
    public DecisionPeriodIndicatorPlugin() {
        super(new LotDecisionPeriodIndicatorPlugin());
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.INTEGRITY_DECISION_PERIOD.name();
    }
}
