package eu.digiwhist.worker.hr.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import static eu.digiwhist.worker.hr.parsed.EOJNTenderFormUtils.PUBLICATION_SOURCE_FORM_TYPE_ANCIENT_SELECTOR;

/**
 * Handler for Croatian ancient publication data.
 *
 * @author Marek Mikes
 */
final class EOJNTenderAncientHandler {
    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderAncientHandler() {
    }

    /**
     * Parse method for Croatian ancient publication data.
     *
     * @param document
     *         document to parse data from
     * @param url
     *         url of the document
     *
     * @return ParsedTender with data added
     */
    static ParsedTender parse(final Document document, final String url) {
        ParsedTender parsedTender = new ParsedTender();

        // we do not parse information about ancient tenders, just publication source form type
        parsedTender
                .addPublication(new ParsedPublication()
                        .setIsIncluded(true)
                        .setHumanReadableUrl(url)
                        .setSource(PublicationSources.HR_EOJN)
                        .setSourceFormType(getPublicationSourceFormTypeElement(document).text().trim()));

        return parsedTender;
    }

    /**
     * Gets publication source form type element. It is useful to get publication source form type and decide whether
     * the publication is old.
     *
     * @param document
     *         parsed document
     *
     * @return publication source form type element
     */
    private static Element getPublicationSourceFormTypeElement(final Document document) {
        return JsoupUtils.selectFirst(PUBLICATION_SOURCE_FORM_TYPE_ANCIENT_SELECTOR, document);
    }

}
