package eu.datlab.worker.in.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.apache.commons.lang3.StringUtils;
import org.jsoup.nodes.Element;

import static eu.datlab.worker.in.parsed.CPPPParserUtils.getValue;

/**
 * Contract award handler.
 *
 * @author Tomas Mrazek
 */
public final class CPPPAwardHandler {

    /**
     * Suppress default constructor.
     */
    private CPPPAwardHandler() {
    }

    /**
     * Updates the given tender with award specific data.
     *
     * @param context
     *      context to be parsed
     * @return parsed tender
     */
    public static ParsedTender parse(final Element context) {
        String supplyOrProcedureType = getValue(context, "Tender Type");

        ParsedTender tender = new ParsedTender()
            .addPublication(new ParsedPublication()
                .setSourceFormType(PublicationFormType.CONTRACT_AWARD.name())
                .setIsIncluded(true)
                .setSource(PublicationSources.IN_CPPP)
                .setSourceTenderId(getValue(context, "Tender (Ref\\. No\\.|Reference Number)"))
                .setPublicationDate(getValue(context, "Published Date")))
            .addBuyer(new ParsedBody()
                .setName(getValue(context, "Organisation Name")))
            .setDescription(getValue(context, "Tender Description"))
            .setDocumentsLocation(new ParsedAddress()
                .setUrl(getValue(context, "Tender Document")))
            .setSupplyType(supplyOrProcedureType)
            .setNationalProcedureType(supplyOrProcedureType)
            .addLot(new ParsedTenderLot()
                .setBidsCount(getValue(context, "Number of bids received"))
                .addBid(new ParsedBid()
                    .setIsWinning(Boolean.TRUE.toString())
                    .addBidder(new ParsedBody()
                        .setName(getValue(context, "Name of the selected bidder\\(s\\)"))
                        .setAddress(new ParsedAddress()
                            .setRawAddress(getValue(context, "Address of the selected bidder\\(s\\)"))))
                    .setPrice(new ParsedPrice().setNetAmount(getValue(context, "Contract Value"))))
                .setContractSignatureDate(getValue(context, "Contract Date")));

        String completionDateOrPeriod = getValue(context, "Date of Completion/Completion Period in Days");
        if (StringUtils.isNumeric(completionDateOrPeriod)) {
            tender.getLots().get(0).setEstimatedDurationInDays(completionDateOrPeriod);
        } else {
            tender.getLots().get(0).setCompletionDate(completionDateOrPeriod);
        }

        return tender;
    }
}
