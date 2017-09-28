package eu.digiwhist.worker.es.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedDocument;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.List;

/**
 * Parser handler used for tender xml call for tender parsing.
 */
public final class PCETenderXmlCallForTenderHandler {
    /**
     * Suppress default constructor for noninstantiability.
     */
    private PCETenderXmlCallForTenderHandler() {
    }

    /**
     * Parses tender XML detail.
     *
     * @param parsedTender parsed tender
     * @param document     parsed document
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        final List<ParsedDocument> documents = new ArrayList<>();

        if (parsedTender.getDocuments() != null) {
            documents.addAll(parsedTender.getDocuments());
        }

        final Elements attachments = JsoupUtils.select("cac|Attachment cbc|URI", document);

        if (attachments != null && !attachments.isEmpty()) {
            for (Element attachment : attachments) {
                documents.add(new ParsedDocument()
                        .setUrl(attachment.text()));
            }
        }

        return parsedTender
                .setDocuments(documents);

    }
}
