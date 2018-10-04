package eu.datlab.worker.hr.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;

import static eu.datlab.worker.hr.parsed.EOJNTenderNewFormUtils.CHECKBOX_TEXT_SELECTOR;
import static eu.datlab.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_I_5_SELECTOR;
import static eu.datlab.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_VI_4_SELECTOR;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

/**
 * New contract notice form parser for Croatia.
 * It parses specific fields of form "Obavijest o nadmetanju"
 *
 * @author Marek Mikes
 */
final class EOJNTenderNewContractNoticeHandler1 {
    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderNewContractNoticeHandler1() {}

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
        final Element subsectionI5 = JsoupUtils.selectFirst(SUBSECTION_I_5_SELECTOR, document);
        final Element subsectionVI4 = JsoupUtils.selectFirst(SUBSECTION_VI_4_SELECTOR + "," +
                "p:contains(VI.4) + p + table", document);

        parsedTender
                .setAppealBodyName(EOJNTenderNewFormUtils.parseTenderAppealBodyName(subsectionVI4));

        if (parsedTender.getBuyers() != null) {
            parsedTender.getBuyers().get(0)
                .addMainActivity(JsoupUtils.selectText(CHECKBOX_TEXT_SELECTOR, subsectionI5));
        }

        return parsedTender;
    }

}
