package eu.digiwhist.worker.ee.parsed;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * Contract award handler for E-procurement in Estonia.
 *
 * @author Tomas Mrazek
 */
public final class EPEContractAwardHandler {

    private static final Logger logger = LoggerFactory.getLogger(EPEContractAwardHandler.class);

    /**
     * Supress default constructor for noninstantiability.
     */
    private EPEContractAwardHandler() {
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

        return EPEParserUtils.parseNoticeAwardCommonData(doc)
            .setDescription(EPEParserUtils.tableValueByLabel("^II\\.1\\.4\\)", context))
            .setCpvs(EPEParserUtils.parseCPVs("^II\\.1\\.5\\)", context))
            .setIsCoveredByGpa(EPEParserUtils.parseBoolean("^II\\.1\\.6\\)", context))
            .addPublication(parseContractNotice(context))
            .setAppealBodyName(EPEParserUtils.parseAppealBodyName("^VI\\.3\\.3\\)", context))
            .setLots(EPEParserUtils.parseLots(
                EPEContractAwardHandler::parseLot,
                JsoupUtils.selectFirst("tr:matches(^V OSA:) + tr", context),
                JsoupUtils.selectFirst("tr:matches(^VI OSA:)", context)))
            .addFunding(EPEParserUtils.parseEUFunding("^VI\\.1\\)", context))
            .setFinalPrice(EPEParserUtils.parsePrice("^II\\.2\\.1\\)", context));
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
        
        return new ParsedTenderLot()
            // text of first row includes number, remove non-digit characters
            .setLotNumber(node.child(0).text().replaceAll("\\D", ""))
            .setTitle(EPEParserUtils.regexValueByLabel("Nimetus", "Nimetus (?<value>.+)", node))
            .setAwardDecisionDate(EPEParserUtils.parseDateTime("^V\\.1\\.1\\)", node))
            .setBidsCount(EPEParserUtils.regexValueByLabel("^V\\.1\\.2\\)", "(?<value>\\d+)", node))
            .addBid(new ParsedBid()
                .setIsWinning(Boolean.TRUE.toString())
                .addBidder(EPEParserUtils.parseBody("^V\\.1\\.3\\)", node))
                .setPrice(EPEParserUtils.parsePrice("^V\\.1\\.4\\)", node))
                .setIsSubcontracted(EPEParserUtils.parseBoolean("^V\\.1\\.5\\)", node))
                .setSubcontractedValue(EPEParserUtils.parsePrice("^V\\.1\\.5\\)", node)))
            .setEstimatedStartDate(EPEParserUtils.parseDateTime("^V\\.1\\.6\\)", node))
            .setSelectionMethod(EPEParserUtils.tableValueByLabel("^V\\.2\\.6\\)", node))
            .setAwardCriteria(EPEParserUtils.parseAwardCriteria("^V\\.2\\.6\\)", node));
    }

    /**
     * @param context
     *      context that includes contract notice data
     * @return contract notice publication
     */
    private static ParsedPublication parseContractNotice(final Element context) {
        String tmp = EPEParserUtils.regexValueByLabel("Teate registreerimisnumber riiklikus riigihangete registris",
            ": (?<value>.+)", context);
        if (tmp == null) {
            return null;
        }

        Matcher m = Pattern.compile("(?<id>\\d+) (?<date>\\d{2}\\.\\d{2}\\.\\d{4})").matcher(tmp);

        if (m.find()) {
            String sourceFormType = EPEParserUtils.tableValueByLabel("^IV\\.3\\.2\\)", context);
            // sometimes sourceFormType row is missing.
            if (sourceFormType == null || sourceFormType.trim().startsWith("Teate registreerimisnumber riiklikus")) {
                sourceFormType = null;
            }

            return new ParsedPublication()
                .setIsIncluded(false)
                .setSourceTenderId(m.group("id"))
                .setPublicationDate(m.group("date"))
                .setSourceFormType(sourceFormType);
        }

        return null;
    }
}
