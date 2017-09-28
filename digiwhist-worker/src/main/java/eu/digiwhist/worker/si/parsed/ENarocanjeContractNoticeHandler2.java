package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.parsed.utils.ParserUtils;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Element;

import java.util.List;

/**
 * Handler for parsing forms "PZPPO1 - ZJNVETPS" and "PZPPO1 - ZJN-2".
 */
final class ENarocanjeContractNoticeHandler2 extends BaseENarocanjeFormInTableHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractNoticeHandler2() {}

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
    public static ParsedTender parse(final ParsedTender tender, final Element form) {
        parseCommonAttributes(tender, form);

        tender
                .setBuyers(updateBuyer(tender.getBuyers(), form))
                .setTitle(getSectionContent("II.1.1", form))
                .setAwardDeadline(ParserUtils.getFromContent(getSectionWithSeparatedNodes("IV.3.2", form), null,
                        " Do:"))
                .setBidDeadline(getSectionContent("IV.2", form))
                .setCpvs(parseTenderCpvs("II.1.4", form))
                .setHasLots(parseIfTenderHasLots("II.1.5", form))
                .setIsFrameworkAgreement(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                        getSectionContent("IV.1.4", form))));

        parseSectionII2(tender, form);

        return tender;
    }

    /**
     * The method sets specific attributes. It updates just one buyer, because the list size is one (we are parsing
     * just one buyer).
     *
     * @param buyers
     *         list of parsed buyers where is one buyer
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return buyer with specific attributes in list
     */
    private static List<ParsedBody> updateBuyer(final List<ParsedBody> buyers, final Element form) {
        assert buyers.size() == 1;

        buyers.get(0)
                .setBuyerType(getSectionContent("I.2", form));

        return buyers;
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
        final Element sectionII2 = getSectionWithSeparatedNodes("II.2", form);

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
            final String suffix = "(od oddaje naročila)";
            assert estimatedDurationInMonths.endsWith(suffix);
            tender
                    .setEstimatedDurationInMonths(estimatedDurationInMonths.substring(0,
                            estimatedDurationInMonths.indexOf(suffix)));
        }
    }

}
