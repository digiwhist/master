package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.StringUtils;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Element;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Handler for parsing forms "PZPPO1 - ZJN-2" and "PZPPO1 - ZJNVETPS".
 */
final class ENarocanjeContractNoticeHandler2 extends BaseENarocanjeFormInTableHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractNoticeHandler2() {}

    /**
     * Parses form specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from form
     */
    public static ParsedTender parse(final ParsedTender tender, final Element form) {
        parseCommonFormInTableAttributes(tender, form);

        final Element sectionIV11 = ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("IV.1.1", form);

        tender
            .setBuyers(ENarocanjeTenderFormInTableUtils.parseBuyerType(tender.getBuyers(), form))
            .setTitle(ENarocanjeTenderFormInTableUtils.getSectionContent("II.1.1", form))
            .setAwardDeadline(ParserUtils.getFromContent(
                ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("IV.3.2", form), null, " Do:"))
            .setBidDeadline(ENarocanjeTenderFormInTableUtils.getSectionContent("IV.2", form))
            .setCpvs(parseTenderCpvs("II.1.4", form))
            .setHasLots(parseIfTenderHasLots("II.1.5", form))
            .setIsFrameworkAgreement(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                ENarocanjeTenderFormInTableUtils.getSectionContent("IV.1.4", form))))
            .setSupplyType(parseSupplyType("II.1.2", form))
            .setDescription(ENarocanjeTenderFormInTableUtils.getSectionContent("II.1.3", form))
            .setAreVariantsAccepted(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                ENarocanjeTenderFormInTableUtils.getSectionContent("II.1.6", form))))
            .setSelectionMethod(StringUtils.removeDotsAtTheEnd(ParserUtils.getFromContent(sectionIV11, null, 6)))
            .setAwardCriteria(parseTenderAwardCriteria(sectionIV11))
            .addEligibleBidLanguage(ENarocanjeTenderFormInTableUtils.getSectionContent("IV.3.1", form))
            .setFundings(parseFundings(form))
            .setIsElectronicAuction(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                ENarocanjeTenderFormInTableUtils.getSectionContent("IV.1.2", form))))
            .setIsDps(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                ENarocanjeTenderFormInTableUtils.getSectionContent("IV.1.3", form))));

        parseSectionII2(tender, form);

        return tender;
    }

    /**
     * Parses section II.2 (there can be some estimated dates or estimated duration info) and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     */
    private static void parseSectionII2(final ParsedTender tender, final Element form) {
        final Element sectionII2 = ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("II.2", form);

        final String estimatedDates = ParserUtils.getFromContent(sectionII2, null, "Začetek");
        if (estimatedDates != null) {
            final String startAndCompletionDatesSeparator = ", Zakljucek";
            assert estimatedDates.contains(startAndCompletionDatesSeparator) && estimatedDates.endsWith(".");
            final String[] startAndCompletionDates = estimatedDates.split(startAndCompletionDatesSeparator);
            tender
                    .setEstimatedStartDate(startAndCompletionDates[0].trim())
                    .setEstimatedCompletionDate(startAndCompletionDates[1].substring(0,
                            startAndCompletionDates[1].length() - 1).trim());
        }

        final String estimatedDurationInMonths = ParserUtils.getFromContent(sectionII2, null, " Trajanje v mesecih:");
        if (estimatedDurationInMonths != null) {
            assert estimatedDurationInMonths.endsWith(ENarocanjeTenderFormUtils.DURATION_IN_DAYS_SUFFIX);
            tender
                    .setEstimatedDurationInMonths(estimatedDurationInMonths.substring(0,
                            estimatedDurationInMonths.indexOf(ENarocanjeTenderFormUtils.DURATION_IN_DAYS_SUFFIX)));
        }
    }

    /**
     * Parses list of fundings.
     *
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return non-empty list of fundings or null
     */
    private static List<ParsedFunding> parseFundings(final Element form) {
        // section number is ">V.1" and the ">" is presented to get yhe section. Otherwise we get section IV.1
        final String sectionTitle =
                "Naročilo se nanaša na projekt in/ali program, ki se financira iz sredstev skupnosti";
        final String isEuFund = ENarocanjeTenderFormInTableUtils.getSectionContent(sectionTitle, form, Arrays.asList(
                sectionTitle));
        if (isEuFund == null) {
            return null;
        }
        return Collections.singletonList(new ParsedFunding()
                .setIsEuFund(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                        isEuFund.contains(".") ? isEuFund.substring(0, isEuFund.indexOf('.')) : isEuFund))));
    }

}
