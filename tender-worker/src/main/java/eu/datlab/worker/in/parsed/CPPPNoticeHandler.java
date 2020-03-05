package eu.datlab.worker.in.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Element;

import static eu.datlab.worker.in.parsed.CPPPParserUtils.getValue;

/**
 * Contract notice handler.
 *
 * @author Tomas Mrazek
 */
public final class CPPPNoticeHandler {

    /**
     * Suppress default constructor.
     */
    private CPPPNoticeHandler() {
    }

    /**
     * Parses tender notice specific data.
     *
     * @param context
     *      context to be parsed
     * @return parsed tender
     */
    public static ParsedTender parse(final Element context) {
        String supplyOrProcedureType = getValue(context, "Tender Type");

        ParsedTender t = new ParsedTender()
            .addPublication(new ParsedPublication()
                .setSourceFormType(PublicationFormType.CONTRACT_NOTICE.name())
                .setIsIncluded(true)
                .setSource(PublicationSources.IN_CPPP)
                .setSourceTenderId(getValue(context, "Tender Reference Number"))
                .setPublicationDate(getValue(context, "ePublished Date")))
            .addBuyer(new ParsedBody()
                .setName(getValue(context, "Organisation Name"))
                .setBuyerType(getValue(context, "Organisation Type"))
                .setContactName(getValue(context, "^Name :$"))
                .setAddress(new ParsedAddress()
                    .setRawAddress(getValue(context, "^Address :$"))))
            .setDescription(getValue(context, "Work Description"))
            .setDocumentsLocation(new ParsedAddress()
                .setUrl(getValue(context, "Tender Document")))
            .setProcedureType(supplyOrProcedureType)
            .setNationalProcedureType(supplyOrProcedureType)
            .setSupplyType(supplyOrProcedureType)
            .setTitle(getValue(context, "Tender Title"))
            .addCpv(new ParsedCPV()
                .setIsMain(Boolean.TRUE.toString())
                .setCode(getValue(context, "Product Category")))
            .addCpv(new ParsedCPV()
                .setIsMain(Boolean.FALSE.toString())
                .setCode(getValue(context, "Product Sub-Category")))
            .setDeposits(getValue(context, "EMD"))
            .setAddressOfImplementation(new ParsedAddress()
                .setRawAddress(getValue(context, "Location")))
            .setDocumentsDeadline(getValue(context, "Document Download End Date"))
            .setBidDeadline(getValue(context, "Bid Submission End Date"));

        return t;
    }
}
