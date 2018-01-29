package eu.digiwhist.worker.lt.parsed;

import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Created by michalriha on 02/06/2017.
 */
public class CVPISTenderParser extends BaseDigiwhistTenderParser {
    private static final String VERSION = "1.0";

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "LT";
    }

    @Override
    public final List<ParsedTender> parse(final RawData raw) {
        final Element document = Jsoup.parse(raw.getSourceData()).select("div#skelbimo_turinys").first();
        final Map<String, Object> metaData = raw.getMetaData();
        final String publicationDate = (String) metaData.get("publicationDate");
        
        ParsedTender parsedTender = CVPISTenderOldHandler.parse(document, raw.getSourceUrl().toString(),
            publicationDate);

        if (parsedTender.getTitle() == null) {
            parsedTender = CVPISTenderNewHandler.parse(document, raw.getSourceUrl().toString(), publicationDate);
        }

        return Collections.singletonList(parsedTender);
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    /**
     * Select not empty text.
     *
     * @param selector selector
     * @param element element to parse from
     *
     * @return String or null
     */
    public static String parseOwnText(final String selector, final Element element) {
        final Elements results = JsoupUtils.select(selector, element);

        if (results == null || results.isEmpty()) {
            return null;
        }

        for (Element result : results) {
            if (result != null && !result.ownText().trim().isEmpty()) {
                return result.ownText();
            }
        }

        return null;
    }
}
