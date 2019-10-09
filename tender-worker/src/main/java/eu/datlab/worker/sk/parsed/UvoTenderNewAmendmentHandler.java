package eu.datlab.worker.sk.parsed;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedAmendment;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.parsePrice;

/**
 * Parser for Uvo Tender amendment form specific data.
 *
 * @author Tomas Mrazek
 */
final class UvoTenderNewAmendmentHandler {
    private static final Logger logger = LoggerFactory.getLogger(UvoTenderNewAmendmentHandler.class.getName());

    /**
     * Suppress default constructor.
     */
    private UvoTenderNewAmendmentHandler() {
    }

    /**
     * Parse method for Uvo amendment form specific data.
     *
     * @param parsedTender tender to add data to
     * @param document     document to parse data from
     * @return ParsedBasicTender with data added
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        if (parsedTender.getLots() == null || parsedTender.getLots().isEmpty()) {
            logger.error("Unable to parse amendment because of tender has no lots.");
            throw new UnrecoverableException("Tender has no lots.");
        } else if (parsedTender.getLots().size() > 1) {
            logger.error("Unable to parse amendment because of tender has more than one lot.");
            throw new UnrecoverableException("Tender has more than one lot.");
        }

        Element sectionVII = JsoupUtils.selectFirst("legend:matchesOwn(ODDIEL VII.*) + div", document);

        Element descriptionNode = ParserUtils.getSubsectionOfElements(
            JsoupUtils.selectFirst("div.subtitle:contains(Opis úprav) + div", sectionVII),
            JsoupUtils.selectFirst("div.subtitle:contains(Opis úprav) ~ div.subtitle", sectionVII)
        );

        Element reasonNode = ParserUtils.getSubsectionOfElements(
            JsoupUtils.selectFirst("div.subtitle:contains(Dôvody na úpravy) + div", sectionVII),
            JsoupUtils.selectFirst("div.subtitle:contains(Dôvody na úpravy) ~ div.subtitle", sectionVII)
        );

        parsedTender.getLots().get(0).addAmendment(new ParsedAmendment()
            .setOriginalPrice(parsePrice(sectionVII,
                "div:containsOwn(Aktualizovaná celková hodnota) > span + span + span",
                "div:containsOwn(Aktualizovaná celková hodnota) > span",
                "div:containsOwn(Aktualizovaná celková hodnota) > span + span", null))
            .setUpdatedPrice(parsePrice(sectionVII,
                "div:containsOwn(Celková hodnota zákazky po úprave) > span + span + span",
                "div:containsOwn(Celková hodnota zákazky po úprave) > span",
                "div:containsOwn(Celková hodnota zákazky po úprave) > span + span", null))
            .setModificationReason(reasonNode != null ? reasonNode.text() : null)
            .setModificationReasonDescription(descriptionNode != null ? descriptionNode.text() : null));

        return parsedTender;
    }
}
