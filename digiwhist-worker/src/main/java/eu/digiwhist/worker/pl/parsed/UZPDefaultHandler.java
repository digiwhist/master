package eu.digiwhist.worker.pl.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import org.jsoup.nodes.Document;

import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

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
     * @param document
     *         xml document
     * @param machineReadableUrl
     *      machine readable URL of included publication
     * @return list of parsed tenders
     */
    public static List<ParsedTender> parse(final Document document, final String machineReadableUrl) {
        Elements nodes = JsoupUtils.select("ogloszenia ogloszenie", document);
        if (nodes != null && !nodes.isEmpty()) {
            return JsoupUtils.select("ogloszenie", document).stream()
                .map(n -> parseTender(n, machineReadableUrl)).collect(Collectors.toList());
        } else {
            return Arrays.asList(parseTender(document, machineReadableUrl));
        }
    }

    /**
     * @param node
     *      tender node
     * @param machineReadableUrl 
     *      machine readable URL of included publication
     * @return parsed tender
     */
    private static ParsedTender parseTender(final Element node, final String machineReadableUrl) {
        return new ParsedTender().addPublication(new ParsedPublication()
            .setIsIncluded(true)
            .setSource(PublicationSources.PL_UZP_FTP)
            .setPublicationDate(JsoupUtils.selectText("data_publikacji", node))
            .setMachineReadableUrl(machineReadableUrl)
            .setSourceId(JsoupUtils.selectText("numer", node))
            .setSourceFormType(JsoupUtils.getRoot(node).tagName().toUpperCase()));
    }
}
