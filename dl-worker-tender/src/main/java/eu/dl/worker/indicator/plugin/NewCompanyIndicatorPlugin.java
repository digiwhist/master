package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;

/**
 * This plugin calculates new company indicator.
 */
public class NewCompanyIndicatorPlugin extends LotAverageIndicatorPlugin {
    /**
     * Constructor sets lot level indicator plugin.
     */
    public NewCompanyIndicatorPlugin() {
        super(new LotNewCompanyIndicatorPlugin());
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.INTEGRITY_NEW_COMPANY.name();
    }
}