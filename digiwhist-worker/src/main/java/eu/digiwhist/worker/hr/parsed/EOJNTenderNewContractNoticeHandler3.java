package eu.digiwhist.worker.hr.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.CHECKBOX_TEXT_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderNewFormUtils.SUBSECTION_I_2_SELECTOR;

/**
 * New contract notice form parser for Croatia.
 * It parses specific fields of form "Poziv na nadmetanje – sektor"
 *
 * @author Marek Mikes
 */
final class EOJNTenderNewContractNoticeHandler3 {
    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderNewContractNoticeHandler3() {}

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
        final Element subsectionI2 = JsoupUtils.selectFirst(SUBSECTION_I_2_SELECTOR, document);

        parsedTender.getBuyers().get(0)
                .addMainActivity(JsoupUtils.selectText(CHECKBOX_TEXT_SELECTOR, subsectionI2));

        return parsedTender;
    }

}
