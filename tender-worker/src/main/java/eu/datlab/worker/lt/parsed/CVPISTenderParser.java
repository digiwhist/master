package eu.datlab.worker.lt.parsed;

import eu.datlab.worker.parser.BaseDatlabTenderParser;
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
public class CVPISTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "LT";
    }

    @Override
    public final List<ParsedTender> parse(final RawData raw) {
        Element document = Jsoup.parse(raw.getSourceData()).select("div.content").first();
        if (document == null) {
            document = Jsoup.parse(raw.getSourceData()).select("body").first();
        }

        final Map<String, Object> metaData = raw.getMetaData();
        final String publicationDate = (String) metaData.get("publicationDate");

        ParsedTender parsedTender;

        if (raw.getSourceUrl().toString().contains("mw.eviesiejipirkimai")) {
            parsedTender = CVPISTenderMWHandler.parse(document, raw.getSourceUrl().toString(), publicationDate);
        } else {

            if ((document.select("table.tblThinBorder").size() + document.select("table.tblThickBorder").size()) < 2
                    && document.selectFirst("u") == null) {
                parsedTender = CVPISTenderCVPPOldHandler.parse(document, raw.getSourceUrl().toString(),
                        publicationDate);
            } else {
                parsedTender = CVPISTenderCVPPNewHandler.parse(document, raw.getSourceUrl().toString(), publicationDate);
            }
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
     * @param element  element to parse from
     * @return String or null
     */
    public static String parseOwnText(final String selector, final Element element) {
        final Elements results = JsoupUtils.select(selector, element);

        if (results == null || results.isEmpty()) {
            return null;
        }

        for (Element result : results) {
            if (result != null) {
                return result.ownText();
            }
        }

        return null;
    }
}
