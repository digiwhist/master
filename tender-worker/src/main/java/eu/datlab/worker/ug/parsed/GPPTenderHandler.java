package eu.datlab.worker.ug.parsed;

import eu.dl.dataaccess.dto.ocds.OCDSRelease;
import eu.dl.dataaccess.dto.ocds.OCDSTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
/**
 * Tender handler.
 */
public final class GPPTenderHandler {

    /**
     * Suppress default constructor.
     */
    private GPPTenderHandler() {
    }

    /**
     * @param t
     *      parsed tender to be updated
     * @param r
     *      OCDS release
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender t, final OCDSRelease r) {
        OCDSTender tender = r.getTender();

        t.setTitle(tender.getTitle())
            .setEstimatedPrice(GPPParserUtils.parsePrice(tender.getValue()))
            .setSupplyType(GPPParserUtils.enumToString(tender.getMainProcurementCategory()))
            .setProcedureType(GPPParserUtils.enumToString(tender.getProcurementMethod()))
            .setNationalProcedureType(tender.getProcurementMethodDetails());

        GPPParserUtils.updateTenderDeadlines(t, tender);

        return t;
    }

}
