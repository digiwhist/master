package eu.datlab.worker.at.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAmendment;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.jsoup.nodes.Element;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Handler for form type KD_8_2_Z3.
 *
 * @author Miroslav Brezik
 */
final class DataGvTenderKD82Z3Handler extends DataGvTenderCommonHandler {

    /**
     * Default constructor suppression.
     */
    private DataGvTenderKD82Z3Handler() {
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
        //CONTRACTING_BODY
        parsedTender.setBuyers(Arrays.asList(
                getNameIdPhoneEmailType(form.selectFirst("CONTRACTING_BODY > ADDRESS_CONTRACTING_BODY"))));

        //OBJECT_CONTRACT
        parseTitle(parsedTender, form);
        List<ParsedCPV> cpvs = form.select("OBJECT_CONTRACT > OBJECT_DESCR > CPV_ADDITIONAL").stream()
                .map(DataGvTenderCommonHandler::getCpv)
                .collect(Collectors.toList());
        if (!cpvs.isEmpty()) {
            parsedTender.setCpvs(cpvs);
        }
        parsedTender.setDescription(getTextFtMultiLines(form.selectFirst("OBJECT_CONTRACT > OBJECT_DESCR > SHORT_DESCR")));

        //MODIFICATIONS_CONTRACT
        Element descriptionProcurement = form.selectFirst("MODIFICATIONS_CONTRACT > DESCRIPTION_PROCUREMENT");
        Element infoModifications = form.selectFirst("MODIFICATIONS_CONTRACT > INFO_MODIFICATIONS");
        ParsedBid bid = new ParsedBid()
                .setBidders(
                        descriptionProcurement.select("CONTRACTOR").stream()
                                .map(DataGvTenderCommonHandler::getContractorType)
                                .collect(Collectors.toList()));
        List<ParsedCPV> amendmentCpvs = new LinkedList<>();
        amendmentCpvs.add(getCpv(descriptionProcurement.selectFirst("CPV_MAIN")));
        amendmentCpvs.addAll(
                descriptionProcurement.select("CPV_ADDITIONAL").stream()
                        .map(DataGvTenderCommonHandler::getCpv)
                        .collect(Collectors.toList()));
        Element modificationReason = infoModifications.selectFirst("ADDITIONAL_NEED, UNFORESEEN_CIRCUMSTANCE");

        ParsedAmendment parsedAmendment = new ParsedAmendment()
                .setDescription(getTextFtMultiLines(descriptionProcurement.selectFirst("SHORT_DESCR")))
                .setOriginalPrice(getVal(infoModifications.selectFirst("VAL_TOTAL_BEFORE")))
                .setUpdatedPrice(getVal(infoModifications.selectFirst("VAL_TOTAL_AFTER")))
                .setCpvs(amendmentCpvs)
                .setModificationReason(modificationReason.tagName())
                .setModificationReasonDescription(getTextFtMultiLines(modificationReason));

        ParsedTenderLot lot = new ParsedTenderLot()
                .setBids(Arrays.asList(bid))
                .setAmendments(Arrays.asList(parsedAmendment));
        parsedTender.setLots(Arrays.asList(lot));

        //ADDITIONAL_CORE_DATA

        return parsedTender;
    }


}
