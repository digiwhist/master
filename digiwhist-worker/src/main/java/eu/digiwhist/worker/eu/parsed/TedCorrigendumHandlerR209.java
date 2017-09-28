package eu.digiwhist.worker.eu.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedCorrigendum;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import java.util.ArrayList;
import java.util.List;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

/**
 * Parser for TED corrigendum form with version R2.0.9.S01.E01 or newer.
 *
 * @author Tomas Mrazek
 */
public final class TedCorrigendumHandlerR209 {

    /**
     * Source for TED publications.
     */
    private static final String PUBLICATIONS_SOURCE = PublicationSources.EU_TED;
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedCorrigendumHandlerR209() {
        throw new AssertionError();
    }

    /**
     * Parses corrigendum form specific data.
     *
     * @param parsedTender
     *         parsed tender
     * @param document
     *         parsed document
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        final Element originNode = TedTenderParserUtils.getOriginNode(document);
        final Element contractNode = TedTenderParserR209Utils.getSectionII(originNode);
        
        parsedTender
            .setTitle(JsoupUtils.selectText("TITLE", contractNode))
            .setCpvs(TedTenderParserR209Utils.parseCpvs(JsoupUtils.selectFirst("CPV_MAIN", contractNode)))
            .setDescription(JsoupUtils.selectText("SHORT_DESCR", contractNode))
            .addBuyer(TedTenderParserUtils.parseBody(JsoupUtils.selectFirst("ADDRESS_CONTRACTING_BODY", originNode))
                .setMainActivities(TedTenderParserUtils.getDefaultMainActivities(document))
                .setBuyerType(TedTenderParserUtils.getDefaultBuyerType(document)))
            .setCorrections(parseCorrections(JsoupUtils.select("CHANGES > CHANGE", originNode)));

        return parsedTender;
    }

    /**
     * @param corringendumNodes
     *      corrections nodes
     * @return non-empty list of corrections or null
     */
    private static List<ParsedCorrigendum> parseCorrections(final Elements corringendumNodes) {
        if (corringendumNodes == null) {
            return null;
        }

        List<ParsedCorrigendum> corrections = new ArrayList<>();
        corringendumNodes.forEach(n -> {
            corrections.add(new ParsedCorrigendum()
                .setOriginal(JsoupUtils.selectText("OLD_VALUE", n))
                .setReplacement(JsoupUtils.selectText("NEW_VALUE", n))
                .setPlaceOfModifiedText(JsoupUtils.selectText("WHERE > LABEL", n))
                .setSectionNumber(JsoupUtils.selectText("WHERE > SECTION", n)));
        });

        return corrections.isEmpty() ? null : corrections;
    }
}
