package eu.datlab.worker.ug.parsed;

import eu.dl.dataaccess.dto.ocds.OCDSPlanning;
import eu.dl.dataaccess.dto.ocds.OCDSRelease;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
/**
 * Planning handler.
 */
public final class GPPPlanningHandler {

    /**
     * Suppress default constructor.
     */
    private GPPPlanningHandler() {
    }

    /**
     * @param t
     *      parsed tender to be updated
     * @param r
     *      OCDS release
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender t, final OCDSRelease r) {
        OCDSPlanning planning = r.getPlanning();

        t.setTitle(planning.getProject())
            .setEstimatedPrice(GPPParserUtils.parsePrice(planning.getBudget().getAmount()));

        return t;
    }

}
