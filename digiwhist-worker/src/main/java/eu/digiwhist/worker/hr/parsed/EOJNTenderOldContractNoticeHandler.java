package eu.digiwhist.worker.hr.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_III_1_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_III_2_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_IV_2_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_IV_3_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_VI_4_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldFormUtils.SUBSECTION_II_1_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldFormUtils.SUBSECTION_II_2_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldFormUtils.SUBSECTION_II_3_SELECTOR;

/**
 * Old contract notice form parser for Croatia.
 *
 * @author Marek Mikes
 */
final class EOJNTenderOldContractNoticeHandler {
    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderOldContractNoticeHandler() {}

    /**
     * Parses data for old contract notice form.
     *
     * @param parsedTender
     *         tender to add data to
     * @param document
     *         document to parse data from
     *
     * @return ParsedTender with parsed data
     */
    static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        final Element subsectionII1 = JsoupUtils.selectFirst(SUBSECTION_II_1_SELECTOR, document);
        final Element subsectionII2 = JsoupUtils.selectFirst(SUBSECTION_II_2_SELECTOR, document);
        final Element subsectionII3 = JsoupUtils.selectFirst(SUBSECTION_II_3_SELECTOR, document);
        final Element subsectionIII1 = JsoupUtils.selectFirst(SUBSECTION_III_1_SELECTOR, document);
        final Element subsectionIII2 = JsoupUtils.selectFirst(SUBSECTION_III_2_SELECTOR, document);
        final Element subsectionIV2 = JsoupUtils.selectFirst(SUBSECTION_IV_2_SELECTOR, document);
        final Element subsectionIV3 = JsoupUtils.selectFirst(SUBSECTION_IV_3_SELECTOR, document);
        final Element subsectionVI4 = JsoupUtils.selectFirst(SUBSECTION_VI_4_SELECTOR, document);

        parsedTender
                .setDescription(EOJNTenderOldFormUtils.parseTenderDescription(subsectionII1, "II.1.5"))
                .setHasLots(EOJNTenderOldAndNewFormUtils.parseIfTenderHasLots(subsectionII1))
                .setAreVariantsAccepted(EOJNTenderOldAndNewFormUtils.parseBooleanFromCheckboxes("AlternPonuda_DA1",
                        "AlternPonuda_NE1", subsectionII1))
                .setEstimatedPrice(parseTenderEstimatedPrice(subsectionII2))
                .setHasOptions(EOJNTenderOldAndNewFormUtils.parseBooleanFromCheckboxes("Opcije_DA1", "Opcije_NE1",
                        subsectionII1))
                .setEstimatedDurationInMonths(JsoupUtils.selectText("a[name=TrajRazMj1] + span", subsectionII3))
                .setEstimatedDurationInDays(JsoupUtils.selectText("a[name=TrajRazD1] + span", subsectionII3))
                .setEstimatedStartDate(JsoupUtils.selectText("a[name=TrajPoc1] + span", subsectionII3))
                .setEstimatedCompletionDate(JsoupUtils.selectText("a[name=TrajKraj1] + span", subsectionII3))
                .setDeposits(JsoupUtils.selectText("p:contains(III.1.1) + p + table", subsectionIII1))
                .setPersonalRequirements(JsoupUtils.selectText("tr:contains(III.2.1) + tr > td > table",
                        subsectionIII2))
                .setTechnicalRequirements(JsoupUtils.selectText("tr:contains(III.2.3) + tr + tr", subsectionIII2))
                .setSelectionMethod(EOJNTenderOldFormUtils.parseTenderSelectionMethod(subsectionIV2))
                .setIsElectronicAuction(EOJNTenderOldAndNewFormUtils.parseBooleanFromCheckboxes("ElekDrazba_DA1",
                        "ElekDrazba_NE1", subsectionIV2))
                .setDocumentsPayable(EOJNTenderOldAndNewFormUtils.parseBooleanFromCheckboxes("DokNapl_DA1",
                        "DokNapl_NE1", subsectionIV3))
                .setBidDeadline(JsoupUtils.selectText("a[name=RokZaDostavu1] + span", subsectionIV3) + ","
                        + JsoupUtils.selectText("a[name=RokZaDostavuSat1] + span", subsectionIV3))
                .addEligibleBidLanguage(JsoupUtils.selectText(
                        "tr:contains(IV.3.6) ~ tr:has(td:first-child input[checked]) > td:last-child", subsectionIV3))
                .setAwardDeadlineDuration(JsoupUtils.selectText("a[name=RokValjPonD1] + span", subsectionIV3))
                .setAppealBodyName(EOJNTenderOldAndNewFormUtils.parseTenderAppealBodyName(subsectionVI4));

        parsedTender.getPublications().get(0)
                .setDispatchDate(JsoupUtils.selectText("a[name=DatSlanjaObjOOSUJ1] + span", document));

        return parsedTender;
    }

    /**
     * Parse tender estimated price value from document.
     *
     * @param subsectionII2
     *         subsection II.2 to be parsed
     *
     * @return tender estimated price or Null
     */
    private static ParsedPrice parseTenderEstimatedPrice(final Element subsectionII2) {
        final ParsedPrice price = new ParsedPrice()
                .setNetAmount(JsoupUtils.selectText("a[name=ProcVrijednost1] + span", subsectionII2));

        final Element currencyElement = JsoupUtils.selectFirst("input[name=Valuta1]", subsectionII2);
        if (currencyElement != null) {
            return price
                    .setCurrency(currencyElement.val());
        } else {
            // e.g. https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=169765
            return price
                    .setCurrency(JsoupUtils.selectText("td:has(a[name=ProcVrijednost1]) + td > p > span:nth-child(2)",
                            subsectionII2));
        }
    }

}
