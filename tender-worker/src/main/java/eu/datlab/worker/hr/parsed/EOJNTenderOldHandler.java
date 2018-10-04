package eu.datlab.worker.hr.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;

import static eu.datlab.worker.hr.parsed.EOJNTenderFormUtils.PUBLICATION_SOURCE_FORM_TYPE_OLD_SELECTOR;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

/**
 * Handler for Croatian old publication data.
 *
 * @author Marek Mikes
 */
final class EOJNTenderOldHandler {
    /**
     * Private constructor to make this class static.
     */
    private EOJNTenderOldHandler() {
    }

    /**
     * Parse method for Croatian old publication data.
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

        Element sourceFormTypeNode = getPublicationSourceFormTypeElement(document);

        // we do not parse information about old tenders, just publication source form type
        parsedTender.addPublication(new ParsedPublication()
            .setIsIncluded(true)
            .setHumanReadableUrl(url)
            .setSource(PublicationSources.HR_EOJN)
            .setSourceFormType(sourceFormTypeNode != null ? sourceFormTypeNode.text().trim() : null));

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
        return JsoupUtils.selectFirst(PUBLICATION_SOURCE_FORM_TYPE_OLD_SELECTOR, document);
    }

}
