package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.parsed.ParsedUnitPrice;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Handler for parsing form CZ03 - Oznámení o výsledku podlimitního zadávacího řízení (Contract award for
 * under-the-threshold tenders).
 */
final class VVZFormCZ03Handler extends VVZCzFormsHandler {
    private static final Logger logger = LoggerFactory.getLogger(VVZFormCZ03Handler.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZFormCZ03Handler() {
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
        final Element sectionIII = VVZTenderParser.getSectionIII(form);
        final Element sectionV = VVZTenderParser.getCZSectionV(form);


        // SECTION I

        // subsection I.1)
        parsedTender.addBuyer(parseBuyer(sectionI));

        // SECTION II

        // subsection II.1.1)
        parsedTender.setTitle(VVZTenderParser.parseTenderTitle(sectionII));

        // subsection II.1.2)
        parsedTender.setSupplyType(VVZTenderParser.parseSupplyType(sectionII))
                .addCpv(parseCpvCode(sectionII))
                .setAddressOfImplementation(parseAddressOfImplementation(sectionII));

        // subsection II.1.3)
        parsedTender.setDescription(parseTenderDescription(sectionII));

        // subsection II.1.6)
        parsedTender = parseFrameworkAgreementAndDPSInfo(sectionII, parsedTender);

        // subsection II.1.8)
        parsedTender.addFunding(VVZTenderParser.parseEuFunding(sectionII));

        // subsection II.2.1)
        parsedTender.setEstimatedPrice(parseEstimatedPrice(sectionII));

        // II.2.2
        parsedTender = parseTenderFinalPriceOrUnitPrice(sectionII, parsedTender);

        // subsection II.4.1)
        parsedTender.setHasLots(VVZTenderParser.parseHasLots(sectionII));

        // SECTION III

        // subsection III.1.1)
        parsedTender = parseProcedureTypeInfo(sectionIII, parsedTender);

        // subsection III.2.1)
        parsedTender.setSelectionMethod(parseSelectionMethod(sectionIII));
        parsedTender.setAwardCriteria(parseAwardCriteria(sectionIII));

        // subsection III.2.2)
        parsedTender.setIsElectronicAuction(VVZTenderParser.parseIsElectronicAuction(sectionIII));

        // SECTION IV
        parsedTender.setLots(parseLotsAwards(form));

        // SECTION V

        // subsection V.1)
        parsedTender.setAdditionalInfo(VVZTenderParser.parseAdditionalInfo(sectionV));

        return parsedTender;
    }

    // ---------------------------------
    // CZ SUBSECTION II.1.6)
    // ---------------------------------

    /**
     * Parses details about framework agreement and DPS.
     *
     * @param sectionII
     *         section II html
     * @param tender
     *         tender to be updated
     *
     * @return tender updated with framework agreement and DPS info
     */
    private static ParsedTender parseFrameworkAgreementAndDPSInfo(final Element sectionII, final ParsedTender tender) {
        final String noticeInvolvesValue = parseNoticeInvolvesValues(sectionII);

        if (noticeInvolvesValue != null) {
            switch (noticeInvolvesValue) {
                case "CONCLUSION_FRAMEWORK_AGREEMENT":
                    tender.setIsFrameworkAgreement(Boolean.TRUE.toString());
                    break;
                case "CONTRACT_FA":
                    tender.setIsFrameworkAgreement(Boolean.TRUE.toString());
                    // create parent publication
                    tender.addPublication(createParentPublication());
                    break;
                case "CONTRACTS_DPS":
                    tender.setIsDps(Boolean.TRUE.toString());
                    break;
                case "CONTRACT_FA_DPS":
                    tender.setIsDps(Boolean.TRUE.toString());
                    // create parent publication
                    tender.addPublication(createParentPublication());
                    break;
                default:
                    break;
            }
        }
        return tender;
    }

    /**
     * Creates empty parent publication which indicates that the actual tender is minitender or DPS pruchase.
     *
     * @return empty parent publication
     */
    private static ParsedPublication createParentPublication() {
        return new ParsedPublication().setIsIncluded(false).setIsParentTender(true);
    }

    // ---------------------------------
    // CZ SUBSECTION II.2.2)
    // ---------------------------------

    /**
     * Parses tender final price.
     *
     * @param sectionII
     *         section II html
     * @param tender
     *         tender to be updated
     *
     * @return tender with final price
     */
    private static ParsedTender parseTenderFinalPriceOrUnitPrice(final Element sectionII, final ParsedTender tender) {
        final String currency = "CZK";

        // unit price
        if (parseIsUnitPrice(sectionII)) {
            final String unit = parseUnitName(sectionII);
            final String netAmount = parseUnitPriceNetAmount(sectionII);
            // we are not setting unit price for the whole tender for now
            //tender.setFinalPrice(new ParsedUnitPrice().setUnitType(unit).setNetAmount(netAmount).setCurrency
            // (currency));
        }

        final String netAmount = parsePriceNetAmount(sectionII);
        tender.setFinalPrice(new ParsedPrice().setNetAmount(netAmount).setCurrency(currency));

        return tender;
    }

    // ---------------------------------
    // CZ SUBSECTION II.2.2) and IV.3.4)
    // ---------------------------------

    /**
     * Parses price net amount.
     *
     * @param priceSection
     *         html with price info
     *
     * @return price net amount
     */
    private static String parsePriceNetAmount(final Element priceSection) {
        return VVZTenderParserUtils.getFieldValue(priceSection, ".*\\.Total\\.ValueFrom$");
    }

    /**
     * Parses whether price is unit price.
     *
     * @param priceSection
     *         html with price info
     *
     * @return true if price is unit price
     */
    private static boolean parseIsUnitPrice(final Element priceSection) {
        return BooleanUtils.toBoolean(VVZTenderParserUtils.isInputFieldChecked(priceSection, ".*\\.UnitPrice$"));
    }

    /**
     * Parses unit price unit name.
     *
     * @param priceSection
     *         html with price info
     *
     * @return unit name of unit price
     */
    private static String parseUnitName(final Element priceSection) {
        return VVZTenderParserUtils.getFieldValue(priceSection, ".*\\.UnitPriceTotal\\.UnitName$");
    }

    /**
     * Parses unit price net amount.
     *
     * @param priceSection
     *         html with price info
     *
     * @return unit price net amount
     */
    private static String parseUnitPriceNetAmount(final Element priceSection) {
        return VVZTenderParserUtils.getFieldValue(priceSection, ".*\\.UnitPriceTotal\\.ValueFrom$");
    }

    // ---------------------------------
    // CZ SUBSECTION III.1.1)
    // ---------------------------------

    /**
     * Parses procedure type info (including reasons for negotiated procedure without publication).
     *
     * @param sectionIII
     *         section III html
     * @param tender
     *         tender to be updated
     *
     * @return tender with procedure type info
     */
    private static ParsedTender parseProcedureTypeInfo(final Element sectionIII, final ParsedTender tender) {
        final String procedureType = VVZTenderParserUtils.getCheckedInputValue(sectionIII, ".*\\.ProcedureType");
        tender.setNationalProcedureType(procedureType);

        if (procedureType != null && procedureType.equalsIgnoreCase("PT_NEGOTIATED_WITHOUT_PUBLICATION")) {
            // parse npwp reason
            tender.addNpwpReason(parseNpwpReason(sectionIII));
        }

        return tender;
    }

    /**
     * Parses reason for negotiated procedure without publication.
     *
     * @param sectionIII
     *         section III html
     *
     * @return reason for negotiated procedure without publication
     */
    private static String parseNpwpReason(final Element sectionIII) {
        return VVZTenderParserUtils.getFieldValue(sectionIII, ".*\\.Justification$");
    }

    // =================================
    // SECTION IV
    // =================================

    /**
     * Parses awards of lots.
     *
     * @param form
     *         form html
     *
     * @return parsed lots (with award info)
     */
    private static List<ParsedTenderLot> parseLotsAwards(final Document form) {
        List<ParsedTenderLot> lots = new ArrayList<>();
        Elements lotsHtmls = VVZTenderParser.getLotsAwardsHtmls(form);

        for (Element lotHtml : lotsHtmls) {
            ParsedTenderLot parsedLot = new ParsedTenderLot().setPositionOnPage(
                    Integer.toString(lotsHtmls.indexOf(lotHtml) + 1));

            // section IV (section start)
            parsedLot.setContractNumber(VVZTenderParser.parseLotAwardContractNumber(lotHtml))
                    .setLotNumber(VVZTenderParser.parseLotAwardNumber(lotHtml))
                    .setTitle(VVZTenderParser.parseLotAwardTitle(lotHtml));

            final String isAwardedString = VVZTenderParser.parseIsLotAwarded(lotHtml);
            parsedLot.setIsAwarded(isAwardedString);

            if (!BooleanUtils.toBoolean(isAwardedString)) {
                // parse cancelled lot info
                // subsection IV.1)
                parsedLot.setCancellationReason(VVZTenderParser.parseLotAwardCancellationReason(lotHtml));
                parsedLot.setCancellationDate(VVZTenderParser.parseFormPublicationDate(form));
            } else {
                // parse awarded lot info
                // subsection IV.2)

                // subsection IV.2.1)
                parsedLot.setDescription(VVZTenderParser.parseLotDescription(lotHtml));

                // subsection IV.2.2)
                parsedLot.addCpv(parseCpvCode(lotHtml))
                        .setAddressOfImplementation(parseLotAddressOfImplementation(lotHtml));

                // subsection IV.3)

                // subsection IV.3.1)
                parsedLot.setContractSignatureDate(VVZTenderParser.parseLotAwardContractSignatureDate(lotHtml));

                // subsection IV.3.2
                parsedLot.setBidsCount(VVZTenderParser.parseLotAwardBidsCount(lotHtml))
                        .setElectronicBidsCount(VVZTenderParser.parseLotAwardElectronicBidsCount(lotHtml));

                // subsection IV.3.3, IV.3.4
                parsedLot.addBid(parseLotAwardWinningBid(lotHtml));
            }
            lots.add(parsedLot);
        }
        return lots;
    }

    /**
     * Parses lot winning bid.
     *
     * @param lotAwardHtml
     *         lot html
     *
     * @return lot winning bid
     */
    private static ParsedBid parseLotAwardWinningBid(final Element lotAwardHtml) {
        ParsedBid winningBid = new ParsedBid().setIsWinning(Boolean.TRUE.toString());

        // subsection IV.3.3)
        winningBid.setBidders(VVZTenderParser.parseLotAwardWinners(lotAwardHtml));

        // subsection IV.3.4)
        winningBid = parseLotAwardBidPrice(lotAwardHtml, winningBid);

        winningBid.setIsConsortium(VVZTenderParser.parseLotAwardBidIsConsortium(lotAwardHtml));

        return winningBid;
    }

    // ---------------------------------
    // SUBSECTION IV.3.4)
    // ---------------------------------

    /**
     * Parses lot bid price.
     *
     * @param lotAwardHtml
     *         lot html
     * @param bid
     *         bid to be updated
     *
     * @return bid with bid price
     */
    private static ParsedBid parseLotAwardBidPrice(final Element lotAwardHtml, final ParsedBid bid) {
        final String currency = "CZK";

        // unit price
        if (parseIsUnitPrice(lotAwardHtml)) {
            final String unit = parseUnitName(lotAwardHtml);
            final String netAmount = parseUnitPriceNetAmount(lotAwardHtml);
            bid.addUnitPrice(new ParsedUnitPrice().setUnitType(unit).setNetAmount(netAmount).setCurrency(currency));
        } else {
            final String netAmount = parsePriceNetAmount(lotAwardHtml);
            bid.setPrice(new ParsedPrice().setNetAmount(netAmount).setCurrency(currency));
        }
        return bid;
    }
}
