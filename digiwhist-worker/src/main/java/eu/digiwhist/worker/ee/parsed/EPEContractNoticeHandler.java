package eu.digiwhist.worker.ee.parsed;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.dataaccess.dto.parsed.BaseParsedTenderLot;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * Contract notice handler for E-procurement in Estonia.
 *
 * @author Tomas Mrazek
 */
public final class EPEContractNoticeHandler {

    private static final Logger logger = LoggerFactory.getLogger(EPEContractNoticeHandler.class);

    /**
     * Supress default constructor for noninstantiability.
     */
    private EPEContractNoticeHandler() {
        throw new AssertionError();
    }

    /**
     * Parses contract notice.
     *
     * @param doc
     *      parsed document
     * @return parsed tender
     */
    public static ParsedTender parse(final Document doc) {
        Element context = EPEParserUtils.getDataTable(doc);

        ParsedTender tender = EPEParserUtils.parseNoticeAwardCommonData(doc)
            .setDocumentsLocation(parseDocumentsLocation(context))
            .setBidsRecipient(parseBidsRecipient(context))
            .setEstimatedPrice(parseEstimatedPrice(context))
            .setDescription(EPEParserUtils.tableValueByLabel("^II\\.1\\.5\\)", context))
            .setCpvs(EPEParserUtils.parseCPVs("^II\\.1\\.6\\)", context))
            .setIsCoveredByGpa(EPEParserUtils.parseBoolean("^II\\.1\\.7\\)", context))
            .setHasLots(EPEParserUtils.parseBoolean("^II\\.1\\.8\\)", context))
            .setAreVariantsAccepted(EPEParserUtils.parseBoolean("^II\\.1\\.9\\)", context))
            .setPersonalRequirements(EPEParserUtils.tableValueByLabel("^III\\.2\\.1\\)", context))
            .setEconomicRequirements(EPEParserUtils.tableValueByLabel("^III\\.2\\.2\\)", context))
            .setTechnicalRequirements(EPEParserUtils.tableValueByLabel("^III\\.2\\.3\\)", context))
            .setDocumentsDeadline(EPEParserUtils.parseDateTime("Hankedokumentide taotlemise või nendele juurdepääsu"
                + " tähtpäev \\(viimane päev\\)", context))
            .setDocumentsPayable(EPEParserUtils.parseBoolean("Dokumendid on tasulised", context))
            .setBidDeadline(EPEParserUtils.tableValueByLabel("^IV\\.3\\.4\\)", context))
            .setDocumentsPrice(EPEParserUtils.parsePrice("^IV\\.3\\.3\\)", context))
            .addEligibleBidLanguage(EPEParserUtils.tableValueByLabel("^IV\\.3\\.6\\)", context))
            .setAwardDeadlineDuration(EPEParserUtils.tableValueByLabel("^IV\\.3\\.7\\)", context))
            .addFunding(EPEParserUtils.parseEUFunding("^VI\\.2\\)", context))
            // dps ????
            .setIsDps(EPEParserUtils.parseBoolean("^VI\\.6\\)", context))
            .setLots(EPEParserUtils.parseLots(EPEContractNoticeHandler::parseLot,
                JsoupUtils.selectFirst("tr:contains(B LISA:) + tr", context), null))
            .setEnvisagedMaxCandidatesCount(EPEParserUtils.tableValueByLabel("^IV\\.1\\.2\\)", context))
            .setAppealBodyName(EPEParserUtils.parseAppealBodyName("^VI\\.4\\.3\\)", context));
        
        tender = (ParsedTender) parseDuration(tender, "HANKELEPINGU VÕI DÜNAAMILISE HANKESÜSTEEMI KESTUS", context);
        
        return tender;
    }

    /**
     * @param context
     *      context that includes tender estimated price data
     * @return tender estimated price 
     */
    private static ParsedPrice parseEstimatedPrice(final Element context) {
        ParsedPrice price = EPEParserUtils.parsePrice("^II\\.1\\.3\\)", context);
        if (price == null) {
            return EPEParserUtils.parsePrice("^II\\.2\\.1\\)", context);
        }
        
        return price;
    }

    /**
     * Parses duration in year or moths according to the data and set appropriate value in the given {@code tender}.
     *
     * @param tender
     *      tender to be updated
     * @param label
     *      label
     * @param context
     *      context that includes duration data
     * @return updated tender
     */
    private static BaseParsedTenderLot parseDuration(final BaseParsedTenderLot tender, final String label,
        final Element context) {

        String duration = EPEParserUtils.tableValueByLabel(label, context);
        if (duration != null) {
            assert duration.contains("kuundes") || duration.contains("aastates") || duration.contains("päevades");
            
            if (duration.contains("aastates")) {
                tender.setEstimatedDurationInYears(duration);
            } else if (duration.contains("kuundes")) {
                tender.setEstimatedDurationInMonths(duration);
            }  else {
                tender.setEstimatedDurationInDays(duration);
            }
        }
        return tender;
    }

    /**
     * @param node
     *      node that includes lot data
     * @return parsed lot or null
     */
    private static ParsedTenderLot parseLot(final Element node) {
        if (node == null) {
            return null;
        }

        ParsedTenderLot lot = new ParsedTenderLot()
            // text of first row includes number, remove non-digit characters
            .setLotNumber(node.child(0).text().replaceAll("\\D", ""))
            .setTitle(EPEParserUtils.regexValueByLabel("NIMETUS", "NIMETUS (?<value>.+)", node))
            .setDescription(EPEParserUtils.tableValueByLabel("1\\)", node))
            .setCpvs(EPEParserUtils.parseCPVs("2\\)", node))
            .setEstimatedPrice(EPEParserUtils.parsePrice("3\\)", node)).setFundings(null)
            .setEligibilityCriteria(EPEParserUtils.tableValueByLabel("8\\)", node));

        return (ParsedTenderLot) parseDuration(lot, "4\\)", node);
    }

     /**
     * @param context
     *      context that includes bids recipient data
     * @return bids recipient or null
     */
    private static ParsedBody parseBidsRecipient(final Element context) {
        String url = EPEParserUtils.regexValueByLabel("Pakkumuste ja osalemistaotluste elektrooniline esitamine",
            "\\(URL\\) (?<value>.+)", context);
       
        return url != null ? new ParsedBody().setAddress(new ParsedAddress().setUrl(url)) : null;
    }

    /**
     * @param context
     *      context that includes documents location
     * @return documents location or null
     */
    private static ParsedAddress parseDocumentsLocation(final Element context) {
        String url = EPEParserUtils.regexValueByLabel("Elektrooniline juurdepääs teabele",
            "\\(URL\\) (?<value>.+)", context);
        
        return url != null ? new ParsedAddress().setUrl(url) : null;
    }
}
