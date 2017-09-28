package eu.digiwhist.worker.pl.parsed;

import org.jsoup.nodes.Document;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * Parser for tender with one publication.
 *
 * @author Tomas Mrazek
 */
public final class UZPDefaultHandler {
    /**
     * Suppress default constructor for noninstantiability.
     */
    private UZPDefaultHandler() {
        throw new AssertionError();
    }

    /**
     * @param parsedTender
     *         parsed tender
     * @param document
     *         xml document
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        ParsedPublication includedPublication = parsedTender.getPublications().get(0);

        return new ParsedTender().addPublication(includedPublication
            .setIsIncluded(false)
            .setSource(PublicationSources.PL_UZP_FTP)
            .setSourceId(JsoupUtils.selectText("numer", document))
            .setSourceFormType(JsoupUtils.getRoot(document).tagName().toUpperCase())
            .setPublicationDate(JsoupUtils.selectText("data_publikacji", document)));
    }
}
