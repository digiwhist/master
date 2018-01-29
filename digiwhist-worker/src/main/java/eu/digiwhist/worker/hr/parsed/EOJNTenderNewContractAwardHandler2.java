package eu.digiwhist.worker.hr.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.CHECKBOX_TEXT_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_I_6_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_VI_4_SELECTOR;

/**
 * New contract award form parser for Croatia.
 * It parses specific fields of form "Obavijest o dodjeli ugovora – sektorska nabava"
 *
 * @author Marek Mikes
 */
final class EOJNTenderNewContractAwardHandler2 {
    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderNewContractAwardHandler2() {}

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
        final Element subsectionI6 = JsoupUtils.selectFirst(SUBSECTION_I_6_SELECTOR, document);
        final Element subsectionVI4 = JsoupUtils.selectFirst(SUBSECTION_VI_4_SELECTOR + "," +
                "p:contains(VI.4) + p + table", document);

        parsedTender
                .setAppealBodyName(EOJNTenderNewFormUtils.parseTenderAppealBodyName(subsectionVI4));

        parsedTender.getBuyers().get(0)
                .addMainActivity(JsoupUtils.selectText(CHECKBOX_TEXT_SELECTOR, subsectionI6));

        return parsedTender;
    }

}
