package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.BaseMasterTenderLot;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.util.Objects;

/**
 * Missing year.
 */
public class YearMissingIndicatorPlugin extends BaseIndicatorPlugin
        implements IndicatorPlugin<MasterTender> {
    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null
                ||
                ((tender.getPublications() == null
                        || tender.getPublications().stream().noneMatch(p -> p != null && p.getPublicationDate() != null)) 
                        && (tender.getLots() == null || tender.getLots().isEmpty()))) {
            return insufficient();
        }
        if ((tender.getPublications() != null
                && tender.getPublications().stream()
                .filter(Objects::nonNull).map(Publication::getPublicationDate).anyMatch(Objects::nonNull))
                || tender.getLots().stream()
                .filter(Objects::nonNull).map(BaseMasterTenderLot::getEstimatedStartDate).anyMatch(Objects::nonNull)) {
            return calculated(100d);
        }
        return calculated(0d);
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.TRANSPARENCY_YEAR_MISSING.name();
    }
}
