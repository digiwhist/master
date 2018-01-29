package eu.digiwhist.worker.ee.parsed;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
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
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

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

        return EPEParserUtils.parseNoticeAwardCommonData(doc, publicationDate)
            .setIsDps(EPEParserUtils.parseBoolean("^VI\\.6\\)", context))
            .setDescription(EPEParserUtils.tableValueByLabel("^II\\.1\\.4\\)", context))
            .setCpvs(EPEParserUtils.parseCPVs("^II\\.1\\.5\\)", context))
            .setIsCoveredByGpa(EPEParserUtils.parseBoolean("^II\\.1\\.6\\)", context))
            .addPublication(parseContractNotice(context))
            .setAppealBodyName(EPEParserUtils.parseAppealBodyName("^VI\\.3\\.3\\)", context))
            .setLots(EPEParserUtils.parseLots(
                EPEContractAwardHandler::parseLot,
                JsoupUtils.selectFirst("tr:matches(^V OSA:) + tr", context),
                JsoupUtils.selectFirst("tr:matches(^VI OSA:)", context),
                Collections.singletonMap("criteria", EPEParserUtils.parseLotsRelatedCriteria(context))))
            .addFunding(EPEParserUtils.parseEUFunding("^VI\\.1\\)", context))
            .setFinalPrice(EPEParserUtils.parsePrice("^II\\.2\\.1\\)", context));
    }

    /**
     * @param node
     *      node that includes lot data
     * @param metaData
     *      meta data
     * @return parsed lot or null
     */
    private static List<ParsedTenderLot> parseLot(final Element node, final Map<String, Object> metaData) {
        if (node == null) {
            return null;
        }

        ParsedTenderLot lot = new ParsedTenderLot()
            // text of first row includes number, remove non-digit characters
            .setLotNumber(JsoupUtils.selectText("tr", node).replaceAll("\\D", ""))
            .setTitle(EPEParserUtils.regexValueByLabel("Nimetus", "(?<value>.+)", node))
            .setAwardDecisionDate(EPEParserUtils.parseDateTime("^V\\.1\\.1\\)", node))
            .setBidsCount(EPEParserUtils.regexValueByLabel("^V\\.1\\.2\\)", "(?<value>\\d+)", node))
            .addBid(new ParsedBid()
                .setIsWinning(Boolean.TRUE.toString())
                .addBidder(EPEParserUtils.parseBody("^V\\.1\\.3\\)", node))
                .setPrice(EPEParserUtils.parsePrice("^V\\.1\\.4\\)", node))
                .setIsSubcontracted(EPEParserUtils.parseBoolean("^V\\.1\\.5\\)", node))
                .setSubcontractedValue(EPEParserUtils.parsePrice("^V\\.1\\.5\\)", node)))
            .setEstimatedCompletionDate(EPEParserUtils.parseDateTime("^V\\.1\\.6\\)", node))
            .setSelectionMethod(EPEParserUtils.tableValueByLabel("^V\\.2\\.6\\)", node))
            .setAwardCriteria(EPEParserUtils.parseAwardCriteria("^V\\.2\\.6\\)", node));


        ObjectMapper mapper = new ObjectMapper();
        String serializedLot = null;
        try {
            serializedLot = mapper.writeValueAsString(lot);
        } catch (JsonProcessingException ex) {
            logger.error("Unable to serialize lot", ex);
            throw new UnrecoverableException("Unable to serialize lot", ex);
        }

        try {
            Map<String, List<ParsedAwardCriterion>> criteria = null;
            if (metaData != null) {
                criteria = (Map<String, List<ParsedAwardCriterion>>) metaData.get("criteria");
            }

            Element relatedPart = JsoupUtils.selectFirst("tr:contains(Seotud hanke osad) + tr", node);
            List<ParsedTenderLot> lots = new ArrayList<>();
            if (relatedPart != null) {
                // for each related part add one lot
                Pattern p = Pattern.compile("Osa (?<number>[0-9]+): (?<title>.+)");
                while (relatedPart != null && p.matcher(relatedPart.text()).matches()) {
                    ParsedTenderLot newLot = mapper.readValue(serializedLot, ParsedTenderLot.class);
                    // set lot number and title from related part
                    Matcher m = p.matcher(relatedPart.text());
                    if (m.find()) {
                        newLot.setLotNumber(m.group("number"))
                            .setTitle(m.group("title"));
                    } else {
                        logger.error("Lot related part has unexpected format, {}", relatedPart.text());
                        throw new UnrecoverableException("Lot related part has unexpected format");
                    }

                    // append lot related award criteria
                    if (criteria != null && newLot.getLotNumber() != null) {
                        newLot.addAwardCriteria(criteria.get(newLot.getLotNumber()));
                    }
                    lots.add(newLot);

                    // attempt to find next related part
                    relatedPart = relatedPart.nextElementSibling();
                }
            } else {
                ParsedTenderLot newLot = mapper.readValue(serializedLot, ParsedTenderLot.class);
                if (criteria != null && newLot.getLotNumber() != null) {
                    newLot.addAwardCriteria(criteria.get(newLot.getLotNumber()));
                }
                lots.add(newLot);
            }

            return lots;
        } catch (IOException ex) {
            logger.error("Unable to deserialize '{}' as lot", serializedLot, ex);
            throw new UnrecoverableException("Unable to deserialize lot", ex);
        }
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
