package eu.datlab.worker.ch.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;
import org.jsoup.select.Elements;

import java.util.Collections;
import java.util.List;

/**
 * Created by michalriha on 19/03/2017.
 */
public class SimapTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1";

    @Override
    public final List<ParsedTender> parse(final RawData raw) {
        final Document document = Jsoup.parse(raw.getSourceData());

        final String headerText = document.select("div.result_head").first().text();
        String publicationDate = null;
        String buyerAssignedId = null;
        String sourceId = null;
        String formType = null;

        if (headerText != null) {
            final String[] headerTextParts = headerText.split("\\|");
            if (headerTextParts.length > 0) {
                publicationDate = headerTextParts[0];
            }

            if (headerTextParts.length > 1) {
                buyerAssignedId = headerTextParts[1];
            }

            if (headerTextParts.length > 2) {
                sourceId = headerTextParts[2];
            }

            if (headerTextParts.length > 3) {
                formType = headerTextParts[3];
            }
        }

        ParsedTender parsedTender = new ParsedTender()
                .setBuyerAssignedId(buyerAssignedId)
                .addPublication(new ParsedPublication()
                        .setIsIncluded(true)
                        .setSource(PublicationSources.CH_SIMAP)
                        .setSourceId(sourceId)
                        .setHumanReadableUrl(raw.getSourceUrl().toString())
                        .setPublicationDate(publicationDate)
                        .setBuyerAssignedId(buyerAssignedId)
                        .setSourceFormType(formType)
                        .setLanguage(parseAnythingRightUnder(new String[]{
                                "span:containsOwn(Langues du dossier d´appel d´offres)",
                                "span:containsOwn(Sprache der Ausschreibungsunterlagen)"}, document)));

        if (formType != null) {
            if (formType.contains("Invitation")) {
                parsedTender = SimapTenderNoticeHandler.parse(parsedTender, document);
            } else if (formType.contains("award")) {
                parsedTender = SimapTenderAwardHandler.parse(parsedTender, document);
            }
        }

        return Collections.singletonList(parsedTender);
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    /**
     * Select text under header.
     *
     * @param selector selector to parse with
     * @param element element to parse from
     * @return String or null
     */
    public static String selectTextUnderHeader(final String selector, final Element element) {
        return selectText("h3:containsOwn(" + selector + ") + dl > dd", element);
    }

    /**
     * Select text that is no null or empty.
     *
     * @param selectors selectors to parse with
     * @param element element to parse from
     * @return String or null
     */
    public static String selectText(final String[] selectors, final Element element) {
        for (String selector : selectors) {
            final String result = selectText(selector, element);

            if (result != null) {
                return result;
            }
        }

        return null;
    }

    /**
     * Select text that is no null or empty.
     *
     * @param selector selector to parse with
     * @param element element to parse from
     * @return String or null
     */
    public static String selectText(final String selector, final Element element) {
        if (element == null) {
            return null;
        }

        final Elements elements = element.select(selector);

        if (elements != null && !elements.isEmpty()) {
            for (Element result : elements) {
                if (!result.ownText().trim().isEmpty()) {
                    return result.ownText().trim();
                }
            }
        }

        return null;
    }

    /**
     * Parse anything (event text node) under given element.
     *
     * @param selector selectors to parse with
     * @param element element to parse from
     * @return String or null
     */
    public static String parseAnythingRightUnder(final String selector, final Element element) {
        final Element sibling = JsoupUtils.selectFirst(selector, element);

        if (sibling == null) {
            return null;
        }

        final int siblingIndex = sibling.siblingIndex() + 1;
        final List<Node> siblings = sibling.parentNode().childNodes();

        if (siblings != null && siblings.size() > siblingIndex && siblings.get(siblingIndex) != null) {
            return siblings.get(siblingIndex).toString();
        } else {
            return null;
        }
    }

    /**
     * Select text that is no null or empty.
     *
     * @param selectors selectors to parse with
     * @param element element to parse from
     * @return String or null
     */
    public static String parseAnythingRightUnder(final String[] selectors, final Element element) {
        for (String selector : selectors) {
            final String result = parseAnythingRightUnder(selector, element);

            if (result != null) {
                return result;
            }
        }

        return null;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "CH";
    }
}
