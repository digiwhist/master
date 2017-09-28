package eu.digiwhist.worker.es.parsed;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.List;

import static eu.dl.worker.utils.jsoup.JsoupUtils.select;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectAttribute;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectText;

/**
 * Parser handler used for tender xml contract award parsing.
 */
public final class PCETenderXmlContractAwardHandler {
    /**
     * Suppress default constructor for noninstantiability.
     */
    private PCETenderXmlContractAwardHandler() {
    }

    /**
     * Parses tender XML detail.
     *
     * @param parsedTender parsed tender
     * @param document     parsed document
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        return parsedTender
                .setLots(parseResultLots(document, parsedTender));

    }

    /**
     * Parse result lots from document.
     *
     * @param document document to parse from
     * @param parsedTender parsed tender
     * @return List<ParsedTenderLot> or null
     */
    private static List<ParsedTenderLot> parseResultLots(final Element document, final ParsedTender parsedTender) {
        final Elements lots = select("cac|TenderResult", document);

        if (lots != null && !lots.isEmpty()) {
            final List<ParsedTenderLot> finalLots = new ArrayList<>();

            for (Element lot : lots) {
                finalLots.add(new ParsedTenderLot()
                        .setStatus(selectText("cbc|ResultCode", lot))
                        .setPositionOnPage(String.valueOf(finalLots.size() + 1))
                        .setSelectionMethod(selectText("cbc|Description", lot))
                        .addAwardCriterion(new ParsedAwardCriterion()
                                .setDescription(selectText("cbc|Description", lot)))
                        .setAwardDecisionDate(selectText("cbc|AwardDate", lot))
                        .setBidsCount(selectText("cbc|ReceivedTenderQuantity", lot))
                        .addBid(new ParsedBid()
                                .setPrice(new ParsedPrice()
                                        .setNetAmount(selectText("cbc|TaxExclusiveAmount", lot))
                                        .setAmountWithVat(selectText("cbc|PayableAmount", lot))
                                        .setCurrency(selectAttribute("cbc|TaxExclusiveAmount", "currencyID", lot))
                                        .setMinNetAmount(selectText("cbc|LowerTenderAmount", lot))
                                        .setMaxNetAmount(selectText("cbc|HigherTenderAmount", lot)))
                                .addBidder(new ParsedBody()
                                        .addBodyId(new BodyIdentifier()
                                                .setId(selectText("cbc|ID", lot))
                                                .setType(BodyIdentifier.Type.TAX_ID)
                                                .setScope(BodyIdentifier.Scope.ES))
                                        .setName(selectText("cbc|Name", lot)))
                                .setIsWinning(String.valueOf(true)))
                        .setLotNumber(selectText("cbc|ProcurementProjectLotID", lot))
                        .setContractSignatureDate(selectText("cbc|IssueDate", lot)));
            }

            if (finalLots.isEmpty()) {
                return null;
            }

            // One lot is sometimes split in two parts, merge these.
            final List<ParsedTenderLot> originalLots = parsedTender.getLots();

            if (originalLots == null || originalLots.isEmpty()) {
                return finalLots;
            }

            for (ParsedTenderLot originalLot : originalLots) {
                int sameLotId = -1;
                for (int i = 0; i < finalLots.size(); i++) {
                    // lot number is not always filled - see https://contrataciondelestado.es/wps/wcm/connect/
                    // PLACE_es/Site/area/docAccCmpnt?srv=cmpnt&cmpntname=GetDocumentsById&source=library
                    // &DocumentIdParam=41f19e5d-3c8b-4b4a-b11d-28dbe0edc82e
                    if (finalLots.get(i).getLotNumber() != null && originalLot.getLotNumber() != null
                            && finalLots.get(i).getLotNumber().trim().equalsIgnoreCase(originalLot.getLotNumber()
                            .trim())) {
                        sameLotId = i;
                        break;
                    }
                }

                // if same lot number found, fill title, cps or lots if not found in first lot
                if (sameLotId != -1) {
                    if (finalLots.get(sameLotId).getTitle() == null) {
                        finalLots.get(sameLotId).setTitle(originalLot.getTitle());
                    }

                    if (finalLots.get(sameLotId).getCpvs() == null || originalLots.get(sameLotId).getCpvs().isEmpty()) {
                        finalLots.get(sameLotId).setCpvs(originalLot.getCpvs());
                    }

                    if (finalLots.get(sameLotId).getEstimatedPrice() == null
                            || finalLots.get(sameLotId).getEstimatedPrice().getNetAmount() == null) {
                        finalLots.get(sameLotId).setEstimatedPrice(originalLot.getEstimatedPrice());
                    }

                    finalLots.get(sameLotId).setPositionOnPage(originalLot.getPositionOnPage());
                    // if no lot with same id found, add whole lot
                } else {
                    finalLots.add(originalLot);
                }
            }

            return finalLots.isEmpty() ? null : finalLots;
        }

        return null;
    }
}
