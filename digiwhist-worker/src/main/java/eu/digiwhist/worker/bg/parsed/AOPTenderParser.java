package eu.digiwhist.worker.bg.parsed;

import java.util.Collections;
import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * Tender parser for AOP in Bulgaria.
 *
 * @author Tomas Mrazek
 */
public final class AOPTenderParser extends BaseDigiwhistTenderParser {

    private static final String VERSION = "1.0";

    @Override
    public List<ParsedTender> parse(final RawData raw) {
        Document doc = Jsoup.parse(raw.getSourceData());

        String sourceFormType = JsoupUtils.selectText("form[name=mainForm] > .form_header > h1, " +
                "form[name=main_form] > .form_header > h1", doc);

        if (sourceFormType != null) {
            return Collections.singletonList(AOPTenderFirstFormHandler.parse(doc, sourceFormType,
                    raw.getSourceUrl().toString()));
        } else {
            final Element formTypeElement = JsoupUtils.select("div.DocumentBody > div", doc).first();
            if (formTypeElement != null && formTypeElement.textNodes().size() > 0 && !formTypeElement.textNodes()
                    .get(0).text().trim().isEmpty()) {
                sourceFormType = formTypeElement.textNodes().get(0).text();
            }

            if (sourceFormType == null) {
                sourceFormType = JsoupUtils.selectText("div.DocumentBody > div > *:eq(0)", doc);
            }

            return Collections.singletonList(AOPTenderSecondFormHandler.parse(doc, sourceFormType,
                    raw.getSourceUrl().toString()));
        }
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "BG";
    }
}
