package eu.datlab.worker.ee.parsed;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import eu.dl.core.UnrecoverableException;

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
     * @param publicationDate
     *      publication date
     * @return parsed tender
     */
    public static ParsedTender parse(final Document doc, final String publicationDate) {
        Element context = EPEParserUtils.getDataTable(doc);
        ParsedTender parsedTender = new ParsedTender();
        parsedTender =  EPEParserUtils.parseNoticeAwardCommonData(doc, publicationDate)
            .setDescription(EPEParserUtils.tableValueByLabel("^II\\.1\\.4\\)", context))
            .setCpvs(EPEParserUtils.parseCPVs("^II\\.1\\.5\\)", context))
            .setIsCoveredByGpa(EPEParserUtils.parseBoolean("^II\\.1\\.6\\)", context))
            .addPublication(parseContractNotice(context))
            .setAppealBodyName(EPEParserUtils.parseAppealBodyName("^VI\\.3\\.3\\)", context))
            .addLots(EPEParserUtils.parseContractAwardsLots(
                EPEContractAwardHandler::parseLot,
                JsoupUtils.selectFirst("tr:matches(^V OSA:) + tr", context),
                JsoupUtils.selectFirst("tr:matches(^VI OSA:)", context)))
            .addFunding(EPEParserUtils.parseEUFunding("^VI\\.1\\)", context))
            .setFinalPrice(EPEParserUtils.parsePrice("^II\\.2\\.1\\)", context));
        return  parsedTender;
    }

    /**
     * The method parses one lot form "contract award".
     *
     * @param node
     *         node that includes lot data
     * @return parsed lot (ParsedTenderLot)
     */
    private static ParsedTenderLot parseLot(final Element node) {

        ParsedTenderLot lot = new ParsedTenderLot();
        StringBuilder title = new StringBuilder();
        String foundTitle = EPEParserUtils.tableValueByLabel("Seotud hanke osad", node);
        String nexRowTitle;
        while (foundTitle != null) {
            title.append(foundTitle);
            nexRowTitle = EPEParserUtils.nextNNodeValueFromNextTableRowByValue(foundTitle, 0, node);
            if (nexRowTitle != null && !nexRowTitle.isEmpty() && !nexRowTitle.equals(" ")) {
                break;
            }
            foundTitle = foundTitle.replace("(", "\\(").replace(")", "\\)");
            foundTitle = EPEParserUtils.tableValueByLabel(foundTitle, node);
        }
        // text of first row includes number, remove non-digit characters
        lot.setLotNumber(JsoupUtils.selectText("tr", node).replaceAll("\\D", ""))
                .setAwardDecisionDate(EPEParserUtils.parseDateTime("^V\\.1\\.1\\)", node))
                .setBidsCount(EPEParserUtils.regexValueByLabel("^V\\.1\\.2\\)", "(?<value>\\d+)", node))
                .setTitle(title.toString())
                .addBid(new ParsedBid()
                        .setIsWinning(Boolean.TRUE.toString())
                        .addBidder(EPEParserUtils.parseBody("^V\\.1\\.3\\)", node))
                        .setIsSubcontracted(EPEParserUtils.parseBoolean("^V\\.1\\.5\\)", node))
                        .setSubcontractedValue(EPEParserUtils.parsePrice("^V\\.1\\.5\\)", node)))
                .setEstimatedCompletionDate(EPEParserUtils.parseDateTime("^V\\.1\\.6\\)", node))
                .setSelectionMethod(EPEParserUtils.tableValueByLabel("^V\\.2\\.6\\)", node))
                .setAwardCriteria(EPEParserUtils.parseAwardCriteria("^V\\.2\\.6\\)", node))
                .setEstimatedPrice(EPEParserUtils.parsePrice("^V\\.1\\.4\\)", node));

        // test on serialisable
        ObjectMapper mapper = new ObjectMapper();
        try {
            mapper.writeValueAsString(lot);
        } catch (JsonProcessingException ex) {
            logger.error("Unable to serialize lot", ex);
            throw new UnrecoverableException("Unable to serialize lot", ex);
        }

        return lot;

    }

    /**
     * @param context
     *      context that includes contract notice data
     * @return contract notice publication
     */
    private static ParsedPublication parseContractNotice(final Element context) {
        String tmp = EPEParserUtils.regexValueByLabel("Teate registreerimisnumber riiklikus riigihangete registris:",
            "(?<value>.+)", context);
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
