package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.time.LocalDate;

/**
 * Missing award decision date.
 */
public class AwardDateMissingIndicatorPlugin extends BaseIndicatorPlugin
        implements IndicatorPlugin<MasterTender> {
    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null) {
            return insufficient();
        }
        LocalDate awardDecisionDate = tender.getAwardDecisionDate();
        return (awardDecisionDate == null)
                ? calculated(0d)
                : calculated(100d);
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.TRANSPARENCY_AWARD_DATE_MISSING.name();
    }
}
