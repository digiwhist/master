package eu.digiwhist.worker.ee.parsed;


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
     * @return parsed tender
     */
    public static ParsedTender parse(final Document doc) {
        Element context = EPEParserUtils.getDataTable(doc);
        
        return EPEParserUtils.parsePublicationAndTitle(doc)
            .addBuyer(EPEParserUtils.parseBuyer(context));
    }
}
