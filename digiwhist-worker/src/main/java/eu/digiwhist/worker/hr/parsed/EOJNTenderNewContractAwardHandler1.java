package eu.digiwhist.worker.hr.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.CHECKBOX_TEXT_SELECTOR;
import static eu.digiwhist.worker.hr.parsed.EOJNTenderOldAndNewFormUtils.SUBSECTION_I_5_SELECTOR;

/**
 * New contract award form parser for Croatia.
 * It parses specific fields of form "Obavijest o dodjeli ugovora"
 *
 * @author Marek Mikes
 */
final class EOJNTenderNewContractAwardHandler1 {
    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderNewContractAwardHandler1() {}

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

        parsedTender.getBuyers().get(0)
                .addMainActivity(JsoupUtils.selectText(CHECKBOX_TEXT_SELECTOR, subsectionI5));

        return parsedTender;
    }

}
