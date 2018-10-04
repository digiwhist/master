package eu.datlab.worker.es.parsed;

import static eu.dl.worker.utils.jsoup.JsoupUtils.select;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectAttribute;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * Parser handler used for tender html detail parsing.
 *
 * @author Tomas Mrazek
 */
public final class PCETenderHtmlDetailHandler {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private PCETenderHtmlDetailHandler() {
    }

    /**
     * Parses tender HTML detail.
     *
     * @param parsedTender
     *         parsed tender
     * @param document
     *         parsed document
     * @param formType
     *         tender type
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document, final String formType) {
        return parsedTender
                .addPublication(new ParsedPublication()
                        .setHumanReadableUrl(JsoupUtils.selectAttribute("href", document.getElementById(
                                "viewns_Z7_AVEQAI930OBRD02JPMTPG21006_:form1:URLgenera")))
                        .setSourceFormType(formType)
                        .setSource(PublicationSources.ES_PCE)
                        .setIsIncluded(true))
                .addPublications(parseRelatedPublications(document));
    }

    /**
     * Parse related publications form document.
     *
     * @param document document to parse from
     * @return List or null
     */
    private static List<ParsedPublication> parseRelatedPublications(final Document document) {
        final List<ParsedPublication> relatedPublications = new ArrayList<>();
        final Elements relatedPublicationsUrls =
                select("table[id=myTablaDetalleVISUOE] > tbody > tr a:containsOwn(Xml)", document);

        if (relatedPublicationsUrls != null && !relatedPublicationsUrls.isEmpty()) {
            for (Element relatedPublicationUrl : relatedPublicationsUrls) {
                relatedPublications.add(new ParsedPublication()
                        .setMachineReadableUrl(selectAttribute("href", relatedPublicationUrl))
                        .setSource(PublicationSources.ES_PCE)
                        .setIsIncluded(false));
            }
        }

        return relatedPublications.isEmpty() ? null : relatedPublications;
    }
}
