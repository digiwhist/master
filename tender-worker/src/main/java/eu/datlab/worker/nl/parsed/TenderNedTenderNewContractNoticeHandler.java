package eu.datlab.worker.nl.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;

import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_II_1_1_TITLE_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_II_1_5_TITLE_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_II_1_6_TITLE_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_II_1_7_TITLE_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_IV_2_2_TITLE_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_IV_2_3_TITLE_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_I_5_TITLE_SELECTOR;

import java.util.ArrayList;
import java.util.List;
import org.jsoup.select.Elements;

/**
 * Parser for TenderNed new contract notice form specific data.
 *
 * @author Marek Mikes
 */
final class TenderNedTenderNewContractNoticeHandler {
    /**
     * Private constructor to make this class static.
     */
    private TenderNedTenderNewContractNoticeHandler() {
    }

    /**
     * Parse method for TenderNed new contract notice form from specific data.
     *
     * @param parsedTender
     *         tender to add data to
     * @param form
     *         document to parse data from
     *
     * @return ParsedTender with data added
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Element form) {
        TenderNedTenderOldAndNewFormUtils.parseCommonAttributes(parsedTender, form);

        parsedTender.getBuyers().get(0).getAddress()
            .setUrl(TenderNedTenderOldAndNewFormUtils.parseNewBuyerUrl(form));

        parsedTender.getBuyers().get(0)
            .setBuyerType(TenderNedTenderOldAndNewFormUtils.parseNewTenderBuyerType(form))
            .setMainActivities(parseBuyerMainActivity(form));

        parsedTender
            .setCpvs(TenderNedTenderOldAndNewFormUtils.parseNewTenderCpvs(form))
            .setHasLots(TenderNedTenderOldAndNewFormUtils.parseIfTenderHasLots(ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_6_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_7_TITLE_SELECTOR, form))))
            .setBidDeadline(TenderNedTenderOldAndNewFormUtils.parseTenderBidDeadline(
                ParserUtils.getSubsectionOfElements(
                    JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_IV_2_2_TITLE_SELECTOR, form),
                    JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_IV_2_3_TITLE_SELECTOR, form))))
            .setEstimatedPrice(parseTenderEstimatedPrice(form))
            .setLots(TenderNedTenderOldAndNewFormUtils.parseNewTenderLots(form))
            .setIsFrameworkAgreement(TenderNedTenderOldAndNewFormUtils.parseNewTenderIsFrameworkAgreement(form))
            .addPublication(TenderNedTenderOldAndNewFormUtils.parseNewTenderPreviousPublicationInTed(form))
            .setSupplyType(TenderNedTenderOldAndNewFormUtils.parseTenderSupplyType(form))
            .setBuyerAssignedId(TenderNedTenderOldAndNewFormUtils.parseBuyerAssignedId(form))
            .setIsCoveredByGpa(TenderNedTenderOldAndNewFormUtils.parseIsTenderCoveredByGpa(form));

        return parsedTender;
    }

    /**
     * Parse buyer main activity value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return non-empty list of activities or null
     */
    private static List<String> parseBuyerMainActivity(final Element form) {
        Elements nodes = JsoupUtils.select("ul > li", ParserUtils.getSubsectionOfElements(
            JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_I_5_TITLE_SELECTOR, form),
            JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_1_TITLE_SELECTOR, form)));

        if (nodes == null || nodes.isEmpty()) {
            return null;
        }

        List<String> activities = new ArrayList<>();
        nodes.forEach(n -> {
            activities.add(n.text());
        });

        return activities;
    }

    /**
     * Parse tender estimated price value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return tender estimated price or Null
     */
    private static ParsedPrice parseTenderEstimatedPrice(final Element form) {
        final Element subsection = ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_5_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_6_TITLE_SELECTOR, form));
        return TenderNedTenderOldAndNewFormUtils.parseNewContractNoticePrice(JsoupUtils.selectText("p", subsection));
    }

}
