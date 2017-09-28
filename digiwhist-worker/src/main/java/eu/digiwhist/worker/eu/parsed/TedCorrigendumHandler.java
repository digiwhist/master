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
 * Parser for TED corrigendum form.
 *
 * @author Tomas Mrazek
 */
public final class TedCorrigendumHandler {

    /**
     * Source for TED publications.
     */
    private static final String PUBLICATIONS_SOURCE = PublicationSources.EU_TED;
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedCorrigendumHandler() {
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

        parsedTender
            .addPublication(TedTenderParserUtils.initMainPublication(document)
                .setSource(PUBLICATIONS_SOURCE))
            .addBuyer(TedTenderParserUtils.parseBody(JsoupUtils.selectFirst("ADDRESS_NOT_STRUCT", originNode))
                .setMainActivities(TedTenderParserUtils.getDefaultMainActivities(document))
                .setBuyerType(TedTenderParserUtils.getDefaultBuyerType(document)))
            .setCorrections(parseCorrections(JsoupUtils.select("CONTENTS > CORREC", originNode)));

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
                .setOriginal(JsoupUtils.selectText("FOR > OLD", n))
                .setReplacement(JsoupUtils.selectText("READ > NEW", n)));
        });

        return corrections.isEmpty() ? null : corrections;
    }
}
