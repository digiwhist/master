package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.util.List;
import java.util.Objects;

/**
 * Missing CPVs.
 */
public class MarketMissingIndicatorPlugin extends BaseIndicatorPlugin
        implements IndicatorPlugin<MasterTender> {

    private static final String CPV_CODE_OTHER = "99000000";

    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if(tender == null) {
            return insufficient();
        }
        List<CPV> cpvs = tender.getCpvs();
        return (cpvs == null || cpvs.isEmpty()
                // if all cpv codes are empty or equal code for OTHER
                || cpvs.stream().filter(Objects::nonNull).map(CPV::getCode).filter(Objects::nonNull)
                .allMatch(c -> c.equals(CPV_CODE_OTHER)))
                ? calculated(0d)
                : calculated(100d);
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.TRANSPARENCY_MARKET_MISSING.name();
    }
}
