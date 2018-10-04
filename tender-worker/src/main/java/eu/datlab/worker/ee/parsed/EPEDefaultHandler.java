package eu.datlab.worker.ee.parsed;


import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import eu.dl.dataaccess.dto.parsed.ParsedTender;

/**
 * Unknown forms handler for E-procurement in Estonia.
 *
 * @author Tomas Mrazek
 */
public final class EPEDefaultHandler {

    /**
     * Supress default constructor for noninstantiability.
     */
    private EPEDefaultHandler() {
        throw new AssertionError();
    }

    /**
     * Parses unknow forms.
     *
     * @param doc
     *      parsed document
     * @param publicationDate
     *      publication date
     * @return parsed tender
     */
    public static ParsedTender parse(final Document doc, final String publicationDate) {
        Element context = EPEParserUtils.getDataTable(doc);
        
        return EPEParserUtils.parsePublicationAndTitle(doc, publicationDate)
            .addBuyer(EPEParserUtils.parseBuyer(context));
    }
}
