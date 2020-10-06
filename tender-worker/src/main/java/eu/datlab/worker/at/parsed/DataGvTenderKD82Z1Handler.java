package eu.datlab.worker.at.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.jsoup.nodes.Element;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Handler for form type KD_8_2_Z1.
 *
 * @author Miroslav Brezik
 */
final class DataGvTenderKD82Z1Handler extends DataGvTenderCommonHandler {

    /**
     * Default constructor suppression.
     */
    private DataGvTenderKD82Z1Handler() {
    }

    /**
     * Parses form specific fields and updates the passed tender.
     *
     * @param parsedTender
     *         tender to be updated with parsed data
     * @param form
     *         parsed XML document from downloaded data
     *
     * @return updated tender object
     */
    public static ParsedTender parseTender(final ParsedTender parsedTender, final Element form) {
        // CONTRACTING_BODY
        parsedTender.setBuyers(Arrays.asList(
                getNameIdPhoneEmailType(form.selectFirst("CONTRACTING_BODY > ADDRESS_CONTRACTING_BODY"))));

        // OBJECT_CONTRACT
        parseTitle(parsedTender, form);
        List<ParsedCPV> cpvs = new LinkedList<>();
        cpvs.add(getCpv(form.selectFirst("OBJECT_CONTRACT > CPV_MAIN")));
        cpvs.addAll(
                form.select("OBJECT_CONTRACT > OBJECT_DESCR > CPV_ADDITIONAL").stream()
                        .map(DataGvTenderCommonHandler::getCpv)
                        .collect(Collectors.toList()));
        parsedTender.setCpvs(cpvs);
        parseTypeContract(parsedTender, form);
        parsedTender.setDescription(getTextFtMultiLines(form.selectFirst("OBJECT_CONTRACT > SHORT_DESCR")));
        Element mainSite = form.selectFirst("OBJECT_CONTRACT > OBJECT_DESCR > MAIN_SITE");
        if (mainSite != null) {
            parsedTender.setAddressOfImplementation(new ParsedAddress().setRawAddress(getTextFtMultiLines(mainSite)));
        }
        parseLots(parsedTender, form);

        // AWARD_CONTRACT
        Element awardedContract = form.selectFirst("AWARD_CONTRACT > AWARDED_CONTRACT");
        parsedTender.setContractSignatureDate(awardedContract.selectFirst("DATE_CONCLUSION_CONTRACT").text());
        ParsedBid bid = new ParsedBid()
                .setPrice(getVal(awardedContract.selectFirst("VAL_TOTAL")))
                .setBidders(
                        awardedContract.select("CONTRACTOR").stream()
                                .map(DataGvTenderCommonHandler::getContractorType)
                                .collect(Collectors.toList()));
        ParsedTenderLot lot = Optional.ofNullable(parsedTender.getLots()).map(l -> l.get(0)).orElse(null);
        if (lot == null) {
            parsedTender.setLots(Arrays.asList(new ParsedTenderLot()));
            lot = parsedTender.getLots().get(0);
        }
        lot.setBidsCount(awardedContract.selectFirst("NB_TENDERS_RECEIVED").text());
        lot.setBids(Arrays.asList(bid));
        Optional.ofNullable(awardedContract.selectFirst("NB_SME_TENDER"))
                .map(Element::text)
                .ifPresent(lot::setSmeBidsCount);

        // PROCEDURE
        parseProcedure(parsedTender, form);

        // ADDITIONAL_CORE_DATA
        parsedTender.setSize(
                form.selectFirst("ADDITIONAL_CORE_DATA > ABOVETHRESHOLD, ADDITIONAL_CORE_DATA > BELOWTHRESHOLD")
                        .tagName());

        return parsedTender;
    }


}
