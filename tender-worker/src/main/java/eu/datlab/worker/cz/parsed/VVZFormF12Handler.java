package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static eu.datlab.worker.cz.parsed.VVZTenderParserUtils.concatenateStrings;
import static eu.datlab.worker.cz.parsed.VVZTenderParserUtils.getFieldValue;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Handler for parsing form F12 - Design contest notice.
 */
final class VVZFormF12Handler extends VVZPriorAndContractNoticeHandler {
    private static final Logger logger = LoggerFactory.getLogger(VVZFormF12Handler.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZFormF12Handler() {
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
        // parsing common attributes from VVZContractNoticeAndPriorNoticeHandler is not called, because this form is
        // a lot different
        ParsedTender parsedTender = tender;

        final Element sectionI = VVZTenderParser.getSectionI(form);
        final Element sectionII = VVZTenderParser.getSectionII(form);
        final Element sectionIV = VVZTenderParser.getSectionIV(form);
        final Element sectionVI = VVZTenderParser.getSectionVI(form);

        parsedTender = parseSectionICommonSubsections(sectionI, parsedTender);

        // subsection I.4, I.5 and I.6
        parsedTender.getBuyers()
                .get(0)
                .setBuyerType(parseBuyerType(sectionI))
                .addMainActivity(parseBuyerMainActivity(sectionI))
                .addMainActivity(parseBuyerMainActivityFromI6(sectionI));

        // subsection II.1.1
        parsedTender.setTitle(VVZTenderParser.parseTenderTitle(sectionII));

        //subsection II.1.2
        parsedTender.setCpvs(parseTenderCPVCodes(sectionII));

        // subsection II.2.4
        parsedTender.setDescription(parseTenderDescriptionFromII24(sectionII));

        // subsection II.2.13
        parsedTender.addFunding(VVZTenderParser.parseEuFunding(sectionII));

        // subsection IV.1.2
        parsedTender = parseEnvisagedCandidatesCountInfo(sectionIV, parsedTender);

        // subsection IV.1.9
        parsedTender.setEligibilityCriteria(parseEligibilityCriteria(sectionIV));

        // subsection IV.2.2
        parsedTender.setBidDeadline(parseBidDeadline(sectionIV));

        // subsection IV.2.4
        parsedTender.setEligibleBidLanguages(parseEligibleBidLanguages(sectionIV));

        // subsection VI.3)
        parsedTender.setAdditionalInfo(VVZTenderParser.parseAdditionalInfo(sectionVI));

        // subsection VI.4.1
        parsedTender.setAppealBodyName(parseAppealBodyName(sectionVI));

        // subsection VI.4.2
        parsedTender.setMediationBodyName(parseMediationBodyName(sectionVI));

        return parsedTender;
    }

    // ---------------------------------
    // SUBSECTION IV.1.2)
    // ---------------------------------

    /**
     * Parses envisaged candidates count info.
     *
     * @param sectionIV
     *         section IV html
     * @param tender
     *         tender to be updated
     *
     * @return updated tender with envisaged candidates count info
     */
    private static ParsedTender parseEnvisagedCandidatesCountInfo(final Element sectionIV, final ParsedTender tender) {
        return tender.setEnvisagedCandidatesCount(parseEnvisagedCandidatesCount(sectionIV))
                .setEnvisagedMinCandidatesCount(parseEnvisagedMinCandidatesCount(sectionIV))
                .setEnvisagedMaxCandidatesCount(parseEnvisagedMaxCandidatesCount(sectionIV));
    }

    /**
     * Parses envisaged candidates count.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return envisaged candidates count
     */
    private static String parseEnvisagedCandidatesCount(final Element sectionIV) {
        return VVZTenderParserUtils.getFieldValue(sectionIV, ".*Participants$");
    }

    /**
     * Parses envisaged minimum candidates count.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return envisaged minimum candidates count
     */
    private static String parseEnvisagedMinCandidatesCount(final Element sectionIV) {
        return VVZTenderParserUtils.getFieldValue(sectionIV, ".*ParticipantsMin$");
    }

    /**
     * Parses envisaged maximum candidates count.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return envisaged maximum candidates count
     */
    private static String parseEnvisagedMaxCandidatesCount(final Element sectionIV) {
        return VVZTenderParserUtils.getFieldValue(sectionIV, ".*ParticipantsMax$");
    }

    // ---------------------------------
    // SUBSECTION IV.2.2)
    // ---------------------------------

    /**
     * Parses bid deadline.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return bid deadline
     */
    private static String parseBidDeadline(final Element sectionIV) {
        final String deadlineDate = getFieldValue(sectionIV, ".*\\.DateReceiptTenders$");
        final String deadlineTime = getFieldValue(sectionIV, ".*\\.TimeReceiptTenders$");
        return concatenateStrings(deadlineDate, deadlineTime);
    }

    // ---------------------------------
    // SUBSECTION IV.2.4)
    // ---------------------------------

    /**
     * Parses eligible bid languages.
     *
     * @param sectionIV
     *         section IV html
     *
     * @return eligible bid languages
     */
    static List<String> parseEligibleBidLanguages(final Element sectionIV) {
        return sectionIV.select("select[name~=.*\\.Languages\\[\\d+\\]$] > option[selected]")
                .stream()
                .filter(bidLang -> !bidLang.attr("value").isEmpty())
                .map(bidLang -> bidLang.attr("value"))
                .collect(Collectors.toList());
    }
}
