package eu.datlab.worker.hr.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;

import static eu.datlab.worker.hr.parsed.EOJNTenderNewFormUtils.CHECKBOX_TEXT_SELECTOR;
import static eu.datlab.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_I_3_SELECTOR;
import static eu.datlab.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_VI_3_SELECTOR;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

/**
 * New contract award form parser for Croatia.
 * It parses specific fields of form "Obavijest o sklopljenim ugovorima"
 *
 * @author Marek Mikes
 */
final class EOJNTenderNewContractAwardHandler3 {
    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderNewContractAwardHandler3() {}

    /**
     * Parses form specific data.
     *
     * @param parsedTender
     *         tender to add data to
     * @param document
     *         document to parse data from
     *
     * @return ParsedTender with parsed data
     */
    static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        final Element subsectionI3 = JsoupUtils.selectFirst(SUBSECTION_I_3_SELECTOR, document);
        final Element subsectionVI3 = JsoupUtils.selectFirst(SUBSECTION_VI_3_SELECTOR, document);

        parsedTender
                .setAppealBodyName(EOJNTenderNewFormUtils.parseTenderAppealBodyName(subsectionVI3));

        if (parsedTender.getBuyers() != null) {
            parsedTender.getBuyers().get(0)
                .addMainActivity(JsoupUtils.selectText(CHECKBOX_TEXT_SELECTOR, subsectionI3));
        }

        return parsedTender;
    }

}
