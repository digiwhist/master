package eu.datlab.worker.eu.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Document;

/**
 * Parser for TED award notice form.
 *
 * @author Tomas Mrazek
 */
public final class TedDefaultHandler {

    /**
     * Source for TED publications.
     */
    private static final String PUBLICATIONS_SOURCE = PublicationSources.EU_TED;

    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedDefaultHandler() {
        throw new AssertionError();
    }

    /**
     * Parses contract award form specific data.
     *
     * @param parsedTender
     *         initialized parsedTender
     * @param document
     *         parsed document
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        parsedTender.addPublication(TedTenderParserUtils.initMainPublication(document));

        TedTenderParserUtils.appendNoticeReference(document, parsedTender);

        return parsedTender;
    }
}
