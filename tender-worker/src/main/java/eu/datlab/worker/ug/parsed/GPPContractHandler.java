package eu.datlab.worker.ug.parsed;

import eu.dl.dataaccess.dto.ocds.OCDSContract;
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

import static eu.datlab.worker.ug.parsed.GPPParserUtils.datetimeToString;
import static eu.datlab.worker.ug.parsed.GPPParserUtils.getContractForAward;
import static eu.datlab.worker.ug.parsed.GPPParserUtils.periodDatetimeToString;

/**
 * Contract handler.
 */
public final class GPPContractHandler {

    /**
     * Suppress default constructor.
     */
    private GPPContractHandler() {
    }

    /**
     * @param t
     *      parsed tender to be updated
     * @param r
     *      OCDS release
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender t, final OCDSRelease r) {
        List<OCDSOrganization> bodies = r.getParties();

        if (r.getAwards() != null) {
            t.setLots(r.getAwards().stream().map(a -> {
                OCDSContract contract = getContractForAward(r, a);

                ParsedTenderLot lot = new ParsedTenderLot()
                    .setTitle(a.getTitle())
                    .setStatus(a.getStatus())
                    .setLotId(a.getId())
                    .setDescription(a.getDescription())
                    .setAwardDecisionDate(datetimeToString(a.getDate()))
                    .setContractSignatureDate(contract != null ? datetimeToString(contract.getSigned()) : null)
                    .addBid(new ParsedBid()
                        .setIsWinning(Boolean.TRUE.toString())
                        .setPrice(GPPParserUtils.parsePrice(a.getValue()))
                        .setBidders(a.getSuppliers().stream()
                            .map(s -> GPPParserUtils.getBody(s, bodies)).collect(Collectors.toList())));

                if (contract != null) {
                    lot.setContractSignatureDate(datetimeToString(contract.getSigned()))
                        .setEstimatedStartDate(periodDatetimeToString(contract.getPeriod(), OCDSPeriod::getStartDate))
                        .setEstimatedCompletionDate(periodDatetimeToString(contract.getPeriod(), OCDSPeriod::getEndDate));

                    lot.getBids().get(0)
                        .addPayment(new ParsedPayment().setPrice(GPPParserUtils.parsePrice(contract.getValue())));
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
