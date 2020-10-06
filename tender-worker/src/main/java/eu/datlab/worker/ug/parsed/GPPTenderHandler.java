package eu.datlab.worker.ug.parsed;

import eu.dl.dataaccess.dto.ocds.OCDSPeriod;
import eu.dl.dataaccess.dto.ocds.OCDSRelease;
import eu.dl.dataaccess.dto.ocds.OCDSTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;

import java.util.Objects;
import java.util.Optional;

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
            .setSupplyType(GPPParserUtils.enumToString(tender.getMainProcurementCategory()))
            .setNationalProcedureType(GPPParserUtils.parseNationalProcedureType(tender))
            .addLot(
                new ParsedTenderLot().setEstimatedPrice(GPPParserUtils.parsePrice(tender.getValue()))
                    .setEstimatedStartDate(GPPParserUtils.periodDatetimeToString(tender.getContractPeriod(), OCDSPeriod::getStartDate))
                    .setEstimatedCompletionDate(GPPParserUtils.periodDatetimeToString(tender.getContractPeriod(), OCDSPeriod::getEndDate))
            ).setEstimatedDurationInDays(Optional.ofNullable(tender.getContractPeriod())
            .map(OCDSPeriod::getDurationInDays).filter(Objects::nonNull).map(String::valueOf).orElse(null))
                // tender.tenderPeriod.endDate contains invalid data, bidDeadline is alternatively parsed as
                // tender.awardPeriod.startDate
            .setBidDeadline(GPPParserUtils.periodDatetimeToString(tender.getAwardPeriod(), OCDSPeriod::getStartDate))
            .setAwardDecisionDate(GPPParserUtils.periodDatetimeToString(tender.getAwardPeriod(), OCDSPeriod::getEndDate));
        // setting publication date for CONTRACT_NOTICE publication
        t.getPublications().get(0).setPublicationDate(GPPParserUtils.periodDatetimeToString(tender.getTenderPeriod(),
                OCDSPeriod::getStartDate));
        return t;
    }

}
