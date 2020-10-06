package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;

/**
 * Missing procedure type.
 */
public class ProcMethodMissingIndicatorPlugin extends BaseIndicatorPlugin
        implements IndicatorPlugin<MasterTender> {
    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null) {
            return insufficient();
        }
        TenderProcedureType procedureType = tender.getProcedureType();
        return (procedureType == null)
                ? calculated(0d)
                : calculated(100d);
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.TRANSPARENCY_PROC_METHOD_MISSING.name();
    }
}
