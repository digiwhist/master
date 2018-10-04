package eu.datlab.worker.pl.parsed;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedCorrigendum;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import java.util.Arrays;

/**
 * Parser for contract notice form specific data.
 *
 * @author Tomas Mrazek
 */
public final class UZPModificationHandler {
    /**
     * Suppress default constructor for noninstantiability.
     */
    private UZPModificationHandler() {
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
        return Arrays.asList(UZPTenderParserUtils.parseCommonFormData(document, machineReadableUrl)
            .addPublication(new ParsedPublication()
                .setIsIncluded(false)
                .setSource(PublicationSources.PL_UZP_FTP)
                .setSourceId(JsoupUtils.selectText("nrpozycji", document))
                .setPublicationDate(JsoupUtils.selectText("datawydaniabiuletynu", document))
                .setSourceFormType(JsoupUtils.selectText("dotyczy", document)))
            .addCorrigendum(new ParsedCorrigendum()
                .setOriginal(JsoupUtils.selectText(":root > jest", document))
                .setReplacement(JsoupUtils.selectText(":root > ma", document)))
            .addCorrections(parseCorrections(JsoupUtils.selectNumberedElements(
                UZPModificationHandler::corrigendumSelectorProducer, JsoupUtils.selectFirst("zmiany", document), 0)))
            .addCorrections(parseCorrections(JsoupUtils.selectNumberedElements(
                UZPModificationHandler::corrigendumSelectorProducer, JsoupUtils.selectFirst("dodane", document), 0))));
    }

    /**
     * For the given number returns corrigendum selector.
     *
     * @param number
     *         number
     *
     * @return selector
     */
    private static String corrigendumSelectorProducer(final int number) {
        return String.format("cz_%1$d", number);
    }

    /**
     * Parses list of corrections from the given nodes.
     *
     * @param corrigendumNodes
     *         list of nodes with corrections data
     *
     * @return list of parsed corrections
     */
    private static List<ParsedCorrigendum> parseCorrections(final Elements corrigendumNodes) {
        if (corrigendumNodes == null || corrigendumNodes.isEmpty()) {
            return null;
        }

        final List<ParsedCorrigendum> corrections = new ArrayList<>();
        for (Element node : corrigendumNodes) {
            corrections.add(new ParsedCorrigendum()
                .setPlaceOfModifiedText(JsoupUtils.selectText("miejsce", node))
                .setOriginal(JsoupUtils.selectText("jest", node))
                .setReplacement(JsoupUtils.selectText("ma, dodac", node)));
        }

        return corrections;
    }
}
