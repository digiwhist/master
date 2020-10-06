package eu.datlab.worker.ug.parsed;

import eu.dl.dataaccess.dto.ocds.OCDSAward;
import eu.dl.dataaccess.dto.ocds.OCDSOrganization;
import eu.dl.dataaccess.dto.ocds.OCDSPeriod;
import eu.dl.dataaccess.dto.ocds.OCDSRelease;
import eu.dl.dataaccess.dto.ocds.OCDSTender;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedPayment;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Award handler.
 */
public final class GPPAwardHandler {

    /**
     * Suppress default constructor.
     */
    private GPPAwardHandler() {
    }

    /**
     * @param t
     *      parsed tender to be updated
     * @param r
     *      OCDS release
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender t, final OCDSRelease r) {
        List<OCDSAward> awards= r.getAwards();

        List<OCDSOrganization> bodies = r.getParties();

        if (awards != null) {
            t.setLots(awards.stream().map(a -> {
                ParsedTenderLot lot = new ParsedTenderLot()
                    .setTitle(a.getTitle())
                    .setStatus(a.getStatus())
                    .setLotId(a.getId())
                    .setDescription(a.getDescription())
                    .setAwardDecisionDate(GPPParserUtils.datetimeToString(a.getDate()))
                    .setEstimatedStartDate(GPPParserUtils.periodDatetimeToString(a.getContractPeriod(),
                            OCDSPeriod::getStartDate))
                    .setEstimatedCompletionDate(GPPParserUtils.periodDatetimeToString(a.getContractPeriod(),
                            OCDSPeriod::getEndDate))
                    .addBid(new ParsedBid()
                        .setIsWinning(Boolean.TRUE.toString())
                        .setPrice(GPPParserUtils.parsePrice(a.getValue()))
                        .setBidders(a.getSuppliers().stream()
                            .map(s -> GPPParserUtils.getBody(s, bodies)).collect(Collectors.toList()))
                        .addPayment(new ParsedPayment().setPrice(GPPParserUtils.parsePrice(a.getValue()))));

                if (r.getTender() != null) {
                    lot.setBidsCount(GPPParserUtils.numberToString(r.getTender().getNumberOfTenderers()));
                }

                return lot;
            }).collect(Collectors.toList()));
        }
        OCDSTender tender = r.getTender();
        if(tender != null) {
            t.setAwardDecisionDate(GPPParserUtils.periodDatetimeToString(tender.getAwardPeriod(), OCDSPeriod::getEndDate));
        }
        return t;
    }

}
