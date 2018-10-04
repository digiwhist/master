package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.List;

/**
 * Handler for parsing form F15 - Voluntary ex ante transparency notice.
 */
final class VVZFormF15Handler extends VVZContractAwardHandler {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZFormF15Handler() {
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
        final Element annexD = getAnnexD(form);

        // SECTION I

        // subsection I.1
        parsedTender.setBuyers(parseBuyers(sectionI));

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

        // subsection II.1.3
        parsedTender.setSupplyType(VVZTenderParser.parseSupplyType(sectionII));

        // subsection II.1.4
        parsedTender.setDescription(parseTenderDescription(sectionII));

        // subsection II.1.6
        parsedTender.setHasLots(VVZTenderParser.parseHasLots(sectionII));

        // subsection II.1.7
        parsedTender.setFinalPrice(parseTenderFinalPrice(sectionII));

        // subsection II.2
        parsedTender.setLots(parseF15LotsFromSubsectionII2(form));

        // SECTION IV

        // subsection IV.1.1
        parsedTender.setNationalProcedureType(VVZTenderParser.parseProcedureType(sectionIV));

        // subsection IV.1.3
        parsedTender.setIsFrameworkAgreement(parseIsFrameworkAgreement(sectionIV));

        // subsection IV.1.8
        parsedTender.setIsCoveredByGpa(parseIsCoveredByGPA(sectionIV));

        // subsection IV.2.1
        parsedTender.addPublication(parsePreviousTedPublication(sectionIV));

        // SECTION V
        parsedTender.addLots(parseF15LotsAwards(form));

        // SECTION VI

        // subsection VI.3)
        parsedTender.setAdditionalInfo(VVZTenderParser.parseAdditionalInfo(sectionVI));

        // subsection VI.4.1
        parsedTender.setAppealBodyName(parseAppealBodyName(sectionVI));

        // subsection VI.4.2
        parsedTender.setMediationBodyName(parseMediationBodyName(sectionVI));

        // attachment D1/D2
        parsedTender.setNpwpReasons(parseNpwpReasons(annexD));

        return parsedTender;
    }

    // =================================
    // SECTION II
    // =================================

    // ---------------------------------
    // SUBSECTION II.2)
    // ---------------------------------

    /**
     * Parses lots.
     *
     * @param form
     *         form html
     *
     * @return parsed lots
     */
    static List<ParsedTenderLot> parseF15LotsFromSubsectionII2(final Document form) {
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
            parsedLot.setAwardCriteria(parseLotAwardCriteria(lotHtml))
                    .addAwardCriteria(parseLotAwardOtherDirectivesCriteria(lotHtml));

            // subsection II.2.11)
            parsedLot.setHasOptions(parseHasLotOptionsFromRadio(lotHtml));

            // subsection II.2.13)
            parsedLot.addFunding(VVZTenderParser.parseEuFunding(lotHtml));

            lots.add(parsedLot);
        }
        return lots;
    }

    /**
     * Parses award criteria for other EU directives.
     *
     * @param lotHtml
     *         lot html
     *
     * @return award criteria for other EU directives
     */
    private static List<ParsedAwardCriterion> parseLotAwardOtherDirectivesCriteria(final Element lotHtml) {
        List<ParsedAwardCriterion> parsedCriteria = new ArrayList<>();

        // parse 2014/23/EU directive criteria
        for (Element criterionHtml : lotHtml.select("div[model~=.*ACCriterionList\\[\\d+\\]]")) {
            final ParsedAwardCriterion parsedCriterion = parseCriterion(criterionHtml, null);
            if (parsedCriterion != null) {
                parsedCriteria.add(parsedCriterion);
            }
        }

        // parse 2009/81/ES directive criteria
        final String isPriceCriterionChecked = VVZTenderParserUtils.getCheckedInputValue(lotHtml, ".*IsCostOrPrice");
        if (isPriceCriterionChecked != null) {
            if (isPriceCriterionChecked.equalsIgnoreCase("AC_PRICE")) {
                parsedCriteria.add(new ParsedAwardCriterion().setIsPriceRelated(Boolean.TRUE.toString())
                        .setWeight(Integer.toString(100))
                        .setName("PRICE"));
            } else {
                for (Element criterionHtml : lotHtml.select("div[model~=.*ACEconomicallyAdvantageousList\\[\\d+\\]]")) {
                    final ParsedAwardCriterion parsedCriterion = parseCriterion(criterionHtml, false);
                    if (parsedCriterion != null) {
                        parsedCriteria.add(parsedCriterion);
                    }
                }
            }
        }
        return parsedCriteria;
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
    private static List<ParsedTenderLot> parseF15LotsAwards(final Document form) {
        List<ParsedTenderLot> lots = new ArrayList<>();
        Elements lotsHtmls = VVZTenderParser.getLotsAwardsHtmls(form);

        for (Element lotHtml : lotsHtmls) {
            ParsedTenderLot parsedLot = new ParsedTenderLot().setPositionOnPage(
                    Integer.toString(lotsHtmls.indexOf(lotHtml) + 1));

            // section V (section start)
            parsedLot.setContractNumber(VVZTenderParser.parseLotAwardContractNumber(lotHtml))
                    .setLotNumber(VVZTenderParser.parseLotAwardNumber(lotHtml))
                    .setTitle(VVZTenderParser.parseLotAwardTitle(lotHtml));

            // parse awarded lot info
            // subsection V.2)

            // subsection V.2.1)
            parsedLot.setContractSignatureDate(VVZTenderParser.parseLotAwardContractSignatureDate(lotHtml));

            // parse bid info (V.2.3, V.2.4, V.2.5)
            parsedLot.addBid(parseLotAwardWinningBid(lotHtml));

            // subsection V.2.4)
            parsedLot.setEstimatedPrice(parseLotAwardEstimatedPrice(lotHtml));

            lots.add(parsedLot);
        }
        return lots;
    }
}
