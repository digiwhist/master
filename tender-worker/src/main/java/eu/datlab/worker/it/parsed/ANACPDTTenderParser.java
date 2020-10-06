package eu.datlab.worker.it.parsed;

import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Parser for "Portale della trasparenza" tenders.
 *
 * @author Tomas Mrazek
 */
public class ANACPDTTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    private static final Logger logger = LoggerFactory.getLogger(ANACPDTTenderParser.class);

    /**
     * Parses the given raw tender object.
     *
     * @param rawTender
     *         raw tender to be parsed
     *
     * @return list of parsed tenders or empty list if none tenders have been parsed
     */
    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        final Document document = Jsoup.parse(rawTender.getSourceData());

        //Sheda gara
        Element tenderNode = document.getElementById("table_grid_K205_0");
        //Scheda Appalto (Lotto)
        Element lotNode = document.getElementById("table_grid_W1114_0");
        //Pubblicazione
        Element publicationNode = document.getElementById("table_grid_K255_0");
        //Dettaglio Aggiudicazione
        Element awardNode = document.getElementById("table_grid_K219_0");
        //Operatori Economici Aggiudicatari
        Element bidsNode = document.getElementById("table_grid_K191_0");
        //Esecuzione
        Element executionNode = document.getElementById("table_grid_K179_0");

        ParsedTender parsedTender = new ParsedTender();

        String cig = JsoupUtils.getFirstValueByLabel(lotNode, "(?i)CIG");

        parsedTender
            .setBuyerAssignedId(JsoupUtils.getFirstValueByLabel(tenderNode, "(?i)Gara"))
            .setTitle(JsoupUtils.getFirstValueByLabel(tenderNode, "(?i)Oggetto Gara"))                        
            .addPublication(new ParsedPublication()
                .setSourceTenderId(cig)
                .setSourceId(cig)
                .setSourceFormType(JsoupUtils.getFirstValueByLabel(lotNode, "(?i)Modalità di"))
                .setIsIncluded(true))
            .setNationalProcedureType(JsoupUtils.getFirstValueByLabel(tenderNode, "(?i)Procedura"))
            .setEstimatedPrice(parsePrice(JsoupUtils.getFirstLabeledValueNode(tenderNode, "(?i)Importo Complessivo")))
            .addBuyer(new ParsedBody()
                .setName(JsoupUtils.getFirstValueByLabel(tenderNode, "(?i)Ragione Sociale SA"))
                .addBodyId(new BodyIdentifier()
                    .setId(JsoupUtils.getFirstValueByLabel(tenderNode, "(?i)Codice Fiscale SA"))
                    .setScope(BodyIdentifier.Scope.IT)
                    .setType(BodyIdentifier.Type.ORGANIZATION_ID))
                .setAddress(new ParsedAddress()
                    .setCity(JsoupUtils.getFirstValueByLabel(tenderNode, "(?i)Località ISTAT SA"))
                    .setUrl(JsoupUtils.getFirstValueByLabel(tenderNode, "(?i)URL SA"))))
            .setBidDeadline(JsoupUtils.getFirstValueByLabel(lotNode, "(?i)Data Scadenza Offerta"))
            .addLot(new ParsedTenderLot()
                .setTitle(JsoupUtils.getFirstValueByLabel(lotNode, "(?i)Oggetto Lotto"))
                .setEstimatedPrice(parsePrice(JsoupUtils.getFirstLabeledValueNode(lotNode,
                    "(?i)Importo a base d\\'asta")))
                .setBidsCount(JsoupUtils.selectText("tr:eq(1) > td:eq(4)", awardNode))
                .setAwardDecisionDate(JsoupUtils.selectText("tr:eq(1) > td:eq(2)", awardNode))
                .addAwardCriterion(new ParsedAwardCriterion()
                    .setName(JsoupUtils.selectText("tr:eq(1) > td:eq(1)", awardNode)))
                .setBids(parseBids(JsoupUtils.select("tr:gt(0)", bidsNode)))
                .setContractSignatureDate(JsoupUtils.selectText("tr:eq(1) > td:eq(1)", executionNode))
                .setCompletionDate(JsoupUtils.selectText("tr:eq(1) > td:eq(4)", executionNode))
                .setStatus(parseLotStatus(executionNode == null ? null : executionNode.getElementById("K165"))))
            .setProcedureType(JsoupUtils.getFirstValueByLabel(lotNode, "(?i)Tipo Appalto"))
            .setAddressOfImplementation(new ParsedAddress()
                .setRawAddress(JsoupUtils.getFirstValueByLabel(lotNode, "(?i)Località ISTAT"))
                .addNuts(JsoupUtils.getFirstValueByLabel(lotNode, "(?i)Località NUTS")))
            .addCpv(new ParsedCPV()
                .setIsMain(Boolean.TRUE.toString())
                .setCode(JsoupUtils.getFirstValueByLabel(lotNode, "(?i)CPV")))
            .setFinalPrice(parsePrice(JsoupUtils.getFirstLabeledValueNode(awardNode, "(?i)Importo a base d\\'asta")))
            .addPublications(parsePublications(JsoupUtils.select("tr:gt(0)", publicationNode)));

        String parentCIG = JsoupUtils.getFirstValueByLabel(tenderNode, "(?i)Riferimento Accordo");
        if (parentCIG != null && !parentCIG.isEmpty()) {
            parsedTender
                .setIsFrameworkAgreement(Boolean.TRUE.toString())
                .addPublication(new ParsedPublication()
                    .setSourceTenderId(parentCIG)
                    .setSourceId(parentCIG)
                    .setIsParentTender(true)
                    .setIsIncluded(false));
        }

        return new ArrayList<>(Arrays.asList(parsedTender));
    }

    /**
     * Parses publications.
     *
     * @param publicationNodes
     *      publication nodes
     * @return list of parsed publications or null
     */
    private List<ParsedPublication> parsePublications(final Elements publicationNodes) {
        if (publicationNodes == null || publicationNodes.isEmpty()) {
            return null;
        }

        List<ParsedPublication> publications = new ArrayList<>();
        for (Element node : publicationNodes) {
            publications.add(new ParsedPublication()
                .setPublicationDate(JsoupUtils.selectText("td:eq(0)", node))
                .setSource(JsoupUtils.selectText("td:eq(2)", node))
                .setSourceId(JsoupUtils.selectText("td:eq(4)", node))
                .setIsIncluded(false));
        }

        return publications;
    }

    /**
     * Parses lot status from text value of the given node.
     *
     * @param statusNode
     *      node that includes status data
     * @return lot status
     */
    private String parseLotStatus(final Element statusNode) {
        if (statusNode == null || statusNode.text().trim().isEmpty()) {
            return null;
        }
        
        return statusNode.text()
            .replaceAll(" \\(.+\\)", "");
    }

    /**
     * Parses bids.
     *
     * @param bidNodes
     *      nodes that include bids data
     * @return list of parsed bids or null
     */
    private List<ParsedBid> parseBids(final Elements bidNodes) {
        if (bidNodes == null || bidNodes.isEmpty()) {
            return null;
        }

        List<ParsedBid> bids = new ArrayList<>();
        for (Element node : bidNodes) {
            bids.add(new ParsedBid()
                .setIsWinning(Boolean.TRUE.toString())
                .addBidder(new ParsedBody()
                    .setName(JsoupUtils.selectText("td:eq(2)", node))
                    .addBodyId(new BodyIdentifier()
                        .setId(JsoupUtils.selectText("td:eq(3)", node))
                        .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                        .setScope(BodyIdentifier.Scope.IT))));
        }

        return bids;
    }

    /**
     * Parses price from the given node.
     *
     * @param priceNode
     *      node that includes price data
     * @return parsed price
     */
    private ParsedPrice parsePrice(final Element priceNode) {
        if (priceNode == null || priceNode.text().trim().isEmpty()) {
            return null;
        }

        String[] priceParts = priceNode.text()
            .replace("€", "EUR")
            .split(" ", 2);

        if (priceParts.length < 2) {
            return new ParsedPrice().setAmountWithVat(priceParts[0]);
        } else {
            return new ParsedPrice()
                .setAmountWithVat(priceParts[1])
                .setCurrency(priceParts[0]);
        }
    }

    /**
     * Returns actual version of this parsed manager.
     *
     * @return actual parsed manager version
     */
    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "IT";
    }
}
