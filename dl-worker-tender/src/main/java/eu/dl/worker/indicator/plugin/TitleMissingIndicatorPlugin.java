package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;

/**
 * Missing title.
 */
public class TitleMissingIndicatorPlugin extends BaseIndicatorPlugin
        implements IndicatorPlugin<MasterTender> {
    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if(tender == null) {
            return insufficient();
        }
        String title = tender.getTitle();
        return (title == null || title.isEmpty())
                ? calculated(0d)
                : calculated(100d);
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.TRANSPARENCY_TITLE_MISSING.name();
    }
}
