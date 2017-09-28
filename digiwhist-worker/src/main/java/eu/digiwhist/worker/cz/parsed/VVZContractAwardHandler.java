package eu.digiwhist.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.List;

/**
 * Handler for Contract Award notices on Vestnik.
 */
abstract class VVZContractAwardHandler extends VVZEuFormsHandler {

    /**
     * Gets annex D.
     *
     * @param form
     *         form html
     *
     * @return annex D html
     */
    static Element getAnnexD(final Document form) {
        return form.select("div#AnnexD").first();
    }

    // =================================
    // SECTION II
    // =================================

    // ---------------------------------
    // SUBSECTION II.1.7)
    // ---------------------------------

    /**
     * Parses tender final price.
     *
     * @param sectionII
     *         section II of the form
     *
     * @return tender final price
     */
    static ParsedPrice parseTenderFinalPrice(final Element sectionII) {
        return parseFinalPrice(sectionII);
    }

    // ---------------------------------
    // SUBSECTION II.2)
    // ---------------------------------

    /**
     * Parses lots information from subsections II.2.
     *
     * @param form
     *         parsed form
     *
     * @return parsed information about lots
     */
    static List<ParsedTenderLot> parseLotsFromSubsectionII2(final Document form) {
        List<ParsedTenderLot> lots = new ArrayList<>();
        Elements lotsHtmls = getLotsHtmls(form);

        for (Element lotHtml : lotsHtmls) {
            ParsedTenderLot parsedLot = new ParsedTenderLot().setPositionOnPage(
                    Integer.toString(lotsHtmls.indexOf(lotHtml) + 1));

            // subsection II.2.1)
            parsedLot.setTitle(VVZTenderParser.parseLotTitle(lotHtml))
                    .setLotNumber(VVZTenderParser.parseLotNumber(lotHtml));

            // subsection II.2.2)
            parsedLot.setCpvs(VVZTenderParser.parseCPVCodes(lotHtml));

            // subsection II.2.3)
            parsedLot.setAddressOfImplementation(parseAddressOfImplementation(lotHtml));

            // subsection II.2.4)
            parsedLot.setDescription(VVZTenderParser.parseLotDescription(lotHtml));

            // subsection II.2.5)
            parsedLot.setAwardCriteria(parseLotAwardCriteria(lotHtml));

            // subsection II.2.11)
            parsedLot.setHasOptions(parseHasLotOptionsFromRadio(lotHtml));

            // subsection II.2.13)
            parsedLot.addFunding(VVZTenderParser.parseEuFunding(lotHtml));

            lots.add(parsedLot);
        }
        return lots;
    }

    // =================================
    // SECTION IV
    // =================================

    // ---------------------------------
    // SUBSECTION IV.2.8)
    // ---------------------------------

    /**
     * Checks whether the form cancels DPS tender and sets cancellation date if so.
     *
     * @param sectionIV
     *         section IV html
     * @param parsedTender
     *         parsed tender to be updated
     * @param form
     *         parsed form
     *
     * @return parsed tender with cancellation date if the form cancels DPS
     */
    static ParsedTender parseDPSCancellationInfo(final Element sectionIV, final ParsedTender parsedTender,
            final Document form) {
        if (parseIsDPSCancelled(sectionIV)) {
            parsedTender.setCancellationDate(VVZTenderParser.parseFormPublicationDate(form));
        }
        return parsedTender;
    }

    /**
     * Parses boolean whether the form cancels DPS tender.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return true if the form cancels DPS
     */
    private static Boolean parseIsDPSCancelled(final Element sectionIV) {
        return VVZTenderParserUtils.isInputFieldChecked(sectionIV, ".*\\.TerminationDPS$");
    }

    // =================================
    // SECTION V
    // =================================

    /**
     * Parses information about awarding lots from section V.
     *
     * @param form
     *         parsed form
     *
     * @return parsed information about awarding lots
     */
    static List<ParsedTenderLot> parseLotsAwards(final Document form) {
        List<ParsedTenderLot> lots = new ArrayList<>();
        Elements lotsHtmls = VVZTenderParser.getLotsAwardsHtmls(form);

        for (Element lotHtml : lotsHtmls) {
            ParsedTenderLot parsedLot = new ParsedTenderLot().setPositionOnPage(
                    Integer.toString(lotsHtmls.indexOf(lotHtml) + 1));

            // section V (section start)
            parsedLot.setContractNumber(VVZTenderParser.parseLotAwardContractNumber(lotHtml))
                    .setLotNumber(VVZTenderParser.parseLotAwardNumber(lotHtml))
                    .setTitle(VVZTenderParser.parseLotAwardTitle(lotHtml));

            final String isAwardedString = VVZTenderParser.parseIsLotAwarded(lotHtml);
            parsedLot.setIsAwarded(isAwardedString);

            if (!BooleanUtils.toBoolean(isAwardedString)) {
                // parse cancelled lot info
                // subsection V.1)
                parsedLot.setCancellationReason(VVZTenderParser.parseLotAwardCancellationReason(lotHtml));
                parsedLot.setCancellationDate(VVZTenderParser.parseFormPublicationDate(form));
            } else {
                // parse awarded lot info
                // subsection V.2)

                // subsection V.2.1)
                parsedLot.setContractSignatureDate(VVZTenderParser.parseLotAwardContractSignatureDate(lotHtml));

                // subsection V.2.2)
                parsedLot.setBidsCount(VVZTenderParser.parseLotAwardBidsCount(lotHtml))
                        .setSmeBidsCount(parseLotAwardSMEBidsCount(lotHtml))
                        .setOtherEuMemberStatesCompaniesBidsCount(parseLotAwardOtherEUBidsCount(lotHtml))
                        .setNonEuMemberStatesCompaniesBidsCount(parseLotAwardNonEUBidsCount(lotHtml))
                        .setElectronicBidsCount(VVZTenderParser.parseLotAwardElectronicBidsCount(lotHtml));

                // parse bid info (V.2.3, V.2.4, V.2.5)
                parsedLot.addBid(parseLotAwardWinningBid(lotHtml));

                // subsection V.2.4)
                parsedLot.setEstimatedPrice(parseLotAwardEstimatedPrice(lotHtml));
            }
            lots.add(parsedLot);
        }
        return lots;
    }

    /**
     * Parses info about lot winning bid.
     *
     * @param lotAwardHtml
     *         html with lot info
     *
     * @return parsed winning bid info
     */
    static ParsedBid parseLotAwardWinningBid(final Element lotAwardHtml) {
        ParsedBid winningBid = new ParsedBid().setIsWinning(Boolean.TRUE.toString());

        // subsection V.2.3)
        winningBid.setBidders(VVZTenderParser.parseLotAwardWinners(lotAwardHtml));

        // subsection V.2.4)
        winningBid.setPrice(parseLotAwardBidPrice(lotAwardHtml));

        // subsection V.2.5)
        winningBid = parseSubcontractingInfo(lotAwardHtml, winningBid);

        // subsection V.2.2)
        winningBid.setIsConsortium(VVZTenderParser.parseLotAwardBidIsConsortium(lotAwardHtml));

        return winningBid;
    }

    // ---------------------------------
    // SUBSECTION V.2)
    // ---------------------------------

    // ---------------------------------
    // SUBSECTION V.2.2) (V.3.2)
    // ---------------------------------

    /**
     * Parses number of SME bids count for lot.
     *
     * @param lotAwardHtml
     *         html with lot info
     *
     * @return number of SME bids count
     */
    static String parseLotAwardSMEBidsCount(final Element lotAwardHtml) {
        return VVZTenderParserUtils.getFieldValue(lotAwardHtml, ".*\\.TendersReceivedSME$");
    }

    /**
     * Parses number of other EU bids count for lot.
     *
     * @param lotAwardHtml
     *         html with lot info
     *
     * @return number of other EU bids count
     */
    static String parseLotAwardOtherEUBidsCount(final Element lotAwardHtml) {
        return VVZTenderParserUtils.getFieldValue(lotAwardHtml, ".*\\.TendersReceivedOtherEU$");
    }

    /**
     * Parses number of non EU bids count for lot.
     *
     * @param lotAwardHtml
     *         html with lot info
     *
     * @return number of non EU bids count
     */
    private static String parseLotAwardNonEUBidsCount(final Element lotAwardHtml) {
        return VVZTenderParserUtils.getFieldValue(lotAwardHtml, ".*\\.TendersReceivedNonEU$");
    }

    // ---------------------------------
    // SUBSECTION V.2.4)
    // ---------------------------------

    /**
     * Parses estimated lot price from lot award section.
     *
     * @param lotAwardHtml
     *         html with lot award info
     *
     * @return estimated lot price
     */
    static ParsedPrice parseLotAwardEstimatedPrice(final Element lotAwardHtml) {
        final String netAmount = VVZTenderParserUtils.getFieldValue(lotAwardHtml, ".*\\.EstimatedTotal\\.ValueFrom$");
        final String currency = VVZTenderParserUtils.getSelectedOptionValue(lotAwardHtml, ".*\\.Currency$");

        if (StringUtils.isNotEmpty(netAmount)) {
            return new ParsedPrice().setNetAmount(netAmount).setCurrency(currency);
        }
        return null;
    }

    /**
     * Parses bid price from lot award section.
     *
     * @param lotAwardHtml
     *         html with lot award info
     *
     * @return bid price
     */
    private static ParsedPrice parseLotAwardBidPrice(final Element lotAwardHtml) {
        return parseFinalPrice(lotAwardHtml);
    }

    // ---------------------------------
    // SUBSECTION V.2.5)
    // ---------------------------------

    /**
     * Parses bid subcontracting info from lot award section.
     *
     * @param lotAwardHtml
     *         html with lot award info
     * @param bid
     *         bid to be updated
     *
     * @return bid with parsed info about subcontracting
     */
    private static ParsedBid parseSubcontractingInfo(final Element lotAwardHtml, final ParsedBid bid) {
        final Boolean isSubcontracted = parseLotAwardBidIsSubcontracted(lotAwardHtml);
        bid.setIsSubcontracted(BooleanUtils.toStringYesNo(isSubcontracted));

        if (BooleanUtils.isTrue(isSubcontracted)) {
            bid.setSubcontractedValue(parseLotAwardBidSubcontractedValue(lotAwardHtml))
                    .setSubcontractedProportion(parseLotAwardBidSubcontractedProportion(lotAwardHtml));
        }

        return bid;
    }

    /**
     * Parses bid subcontracting boolean.
     *
     * @param lotAwardHtml
     *         html with lot award info
     *
     * @return bid with parsed info about subcontracting
     */
    private static Boolean parseLotAwardBidIsSubcontracted(final Element lotAwardHtml) {
        return VVZTenderParserUtils.isInputFieldChecked(lotAwardHtml, ".*\\.LikelySubcontracted$");
    }

    /**
     * Parses bid subcontracted value from lot award section.
     *
     * @param lotAwardHtml
     *         html with lot award info
     *
     * @return subcontracted value
     */
    private static ParsedPrice parseLotAwardBidSubcontractedValue(final Element lotAwardHtml) {
        final String netAmount = VVZTenderParserUtils.getFieldValue(lotAwardHtml,
                ".*\\.SubcontractingValue\\.ValueFrom$");
        final String currency = VVZTenderParserUtils.getSelectedOptionValue(lotAwardHtml,
                ".*\\.SubcontractingValue\\.Currency$");

        if (StringUtils.isNotEmpty(netAmount)) {
            return new ParsedPrice().setNetAmount(netAmount).setCurrency(currency);
        }
        return null;
    }

    /**
     * Parses bid subcontracted proportion from lot award section.
     *
     * @param lotAwardHtml
     *         html with lot award info
     *
     * @return subcontracted proportion
     */
    private static String parseLotAwardBidSubcontractedProportion(final Element lotAwardHtml) {
        return VVZTenderParserUtils.getFieldValue(lotAwardHtml, ".*\\.SubcontractingPCT$");
    }

    // =================================
    // ATTACHMENT D
    // =================================

    /**
     * Parses NPWP reasons from annex D.
     *
     * @param annexD
     *         annex D html
     *
     * @return list of NPWP reasons
     */
    static List<String> parseNpwpReasons(final Element annexD) {
        final List<String> npwpReasons = new ArrayList<>();
        if (annexD != null) {
            for (final Element checkedInput : annexD.select("div.iform-field > input[checked]")) {

                final String inputName = checkedInput.attr("name");
                final String inputType = checkedInput.attr("type");

                // we don't want the top level radio value into npwp reasons
                if (inputName.endsWith("AccordanceArticle")) {
                    continue;
                }

                // we don't care about the detail for "Žádné nabídky nebo žádné vhodné nabídky/žádosti o účast v
                // návaznosti na" and "Nákup dodávek nebo služeb za obzvláště výhodných podmínek"
                if ((inputName.endsWith("ProcedureType") || inputName.endsWith(
                        "PurchaseType")) && inputType.equalsIgnoreCase("radio")) {
                    continue;
                }

                // we care about the detail for "Práce, dodávky nebo služby mohou být poskytovány pouze konkrétním
                // hospodářským subjektem z důvodu" reason
                if (inputName.endsWith("Reason") && inputType.equalsIgnoreCase("checkbox")) {
                    continue;
                }

                final String npwpReason;
                // for radio input, parse its value as npwp reason
                // for checkbox input, parse its label as npwp reason
                if (inputType.equalsIgnoreCase("radio")) {
                    npwpReason = VVZTenderParserUtils.getElementContent(checkedInput);
                } else {
                    npwpReason = inputName;
                }
                npwpReasons.add(npwpReason);
            }
        }
        return npwpReasons;
    }

    // *********************************
    // COMMON UTILS METHODS
    // *********************************

    /**
     * Parses final price.
     *
     * @param priceSection
     *         html segment with price info
     *
     * @return parsed price
     */
    private static ParsedPrice parseFinalPrice(final Element priceSection) {
        final String netAmount = VVZTenderParserUtils.getFieldValue(priceSection, ".*\\.Total\\.ValueFrom$");
        final String currency = VVZTenderParserUtils.getSelectedOptionValue(priceSection, ".*\\.Currency$");

        if (StringUtils.isNotEmpty(netAmount)) {
            return new ParsedPrice().setNetAmount(netAmount).setCurrency(currency);
        }

        // range amounts
        final String minNetAmount = VVZTenderParserUtils.getFieldValue(priceSection, ".*\\.RangeTotal\\.ValueFrom$");
        final String maxNetAmount = VVZTenderParserUtils.getFieldValue(priceSection, ".*\\.RangeTotal\\.ValueTo$");

        if (StringUtils.isNotEmpty(minNetAmount) || StringUtils.isNotEmpty(maxNetAmount)) {
            return new ParsedPrice().setMinNetAmount(minNetAmount).setMaxNetAmount(maxNetAmount).setCurrency(currency);
        }

        return null;
    }
}
