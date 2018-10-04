package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Handler for parsing form F13 - Results of design contest.
 */
final class VVZFormF13Handler extends VVZContractAwardHandler {
    private static final Logger logger = LoggerFactory.getLogger(VVZFormF13Handler.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZFormF13Handler() {
        throw new AssertionError();
    }

    /**
     * Parses Form specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from Form
     */
    public static ParsedTender parseFormAttributes(final ParsedTender tender, final Document form) {
        ParsedTender parsedTender = tender;

        final Element sectionI = VVZTenderParser.getSectionI(form);
        final Element sectionII = VVZTenderParser.getSectionII(form);
        final Element sectionIV = VVZTenderParser.getSectionIV(form);
        final Element sectionVI = VVZTenderParser.getSectionVI(form);

        // SECTION I

        // subsection I.1
        parsedTender.setBuyers(parseBuyers(sectionI));

        // subsection I.2
        parsedTender.setIsCentralProcurement(parseCentralProcurement(sectionI))
                .setIsJointProcurement(parseJointProcurement(sectionI));

        // subsection I.4, I.5 and I.6
        parsedTender.getBuyers()
                .get(0)
                .setBuyerType(parseBuyerType(sectionI))
                .addMainActivity(parseBuyerMainActivity(sectionI))
                .addMainActivity(parseBuyerMainActivityFromI6(sectionI));

        // SECTION II

        // subsection II.1.1
        parsedTender.setTitle(VVZTenderParser.parseTenderTitle(sectionII));

        // subsection II.1.2
        parsedTender.setCpvs(parseTenderCPVCodes(sectionII));

        // subsection II.2.4
        parsedTender.setDescription(parseTenderDescriptionFromII24(sectionII));

        // subsection II.2.13
        parsedTender.addFunding(VVZTenderParser.parseEuFunding(sectionII));

        // SECTION IV

        // subsection IV.1.9
        parsedTender.setEligibilityCriteria(parseEligibilityCriteria(sectionIV));

        // subsection IV.2.1
        parsedTender.addPublication(parsePreviousTedPublication(sectionIV));

        // SECTION V
        parsedTender.addLot(parseLotAward(form));

        // SECTION VI

        // subsection VI.3)
        parsedTender.setAdditionalInfo(VVZTenderParser.parseAdditionalInfo(sectionVI));

        // subsection VI.4.1
        parsedTender.setAppealBodyName(parseAppealBodyName(sectionVI));

        // subsection VI.4.2
        parsedTender.setMediationBodyName(parseMediationBodyName(sectionVI));

        return parsedTender;
    }

    // =================================
    // SECTION V
    // =================================

    /**
     * Parses lots awards.
     *
     * @param form
     *         form html
     *
     * @return lots with award info
     */
    private static ParsedTenderLot parseLotAward(final Document form) {
        ParsedTenderLot lot = new ParsedTenderLot().setPositionOnPage(Integer.toString(1));
        // design contest has only one lot
        final Element lotHtml = VVZTenderParser.getLotsAwardsHtmls(form).first();

        final String isAwardedString = VVZTenderParser.parseIsLotAwarded(lotHtml);
        lot.setIsAwarded(isAwardedString);

        if (!BooleanUtils.toBoolean(isAwardedString)) {
            // parse cancelled lot info
            // subsection V.1)
            lot.setCancellationReason(VVZTenderParser.parseLotAwardCancellationReason(lotHtml));
            lot.setCancellationDate(VVZTenderParser.parseFormPublicationDate(form));
        } else {
            // parse awarded lot info
            // subsection V.3)

            // subsection V.3.1)
            lot.setContractSignatureDate(VVZTenderParser.parseLotAwardContractSignatureDate(lotHtml));

            // subsection V.3.2)
            lot.setBidsCount(VVZTenderParser.parseLotAwardBidsCount(lotHtml))
                    .setSmeBidsCount(parseLotAwardSMEBidsCount(lotHtml))
                    .setForeignCompaniesBidsCount(parseLotAwardOtherEUBidsCount(lotHtml));

            // parse bid info (V.3.3, V.3.4)
            lot.addBid(parseF13LotAwardWinningBid(lotHtml));
        }
        return lot;
    }

    /**
     * Parses lot winning bid.
     *
     * @param lotHtml
     *         lot html
     *
     * @return winning bid
     */
    private static ParsedBid parseF13LotAwardWinningBid(final Element lotHtml) {
        ParsedBid winningBid = new ParsedBid().setIsWinning(Boolean.TRUE.toString());

        // subsection V.3.3)
        winningBid.setBidders(VVZTenderParser.parseLotAwardWinners(lotHtml));

        // subsection V.3.4)
        winningBid.setPrice(parseLotAwardValuePrice(lotHtml));

        return winningBid;
    }

    // ---------------------------------
    // SUBSECTION V.3.4
    // ---------------------------------

    /**
     * Parses lot value price.
     *
     * @param lotHtml
     *         lot html
     *
     * @return value price (bid price)
     */
    private static ParsedPrice parseLotAwardValuePrice(final Element lotHtml) {
        final String netAmount = VVZTenderParserUtils.getFieldValue(lotHtml, ".*\\.ValuePrice\\.ValueFrom$");
        final String currency = VVZTenderParserUtils.getSelectedOptionValue(lotHtml, ".*\\.ValuePrice\\.Currency$");

        if (StringUtils.isNotEmpty(netAmount)) {
            return new ParsedPrice().setNetAmount(netAmount).setCurrency(currency);
        }
        return null;
    }
}
