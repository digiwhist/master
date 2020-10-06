package eu.datlab.worker.in.parsed;

import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import java.util.Collections;
import java.util.List;

/**
 * Tender parser for India.
 *
 * @author Tomas Mrazek
 */
public class CPPPTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "IN";
    }

    @Override
    public final List<ParsedTender> parse(final RawData raw) {
        final Document document = Jsoup.parse(raw.getSourceData());

        Element content = document.getElementById("content");
        if (content == null) {
            logger.error("Raw data doesn't include content node");
            throw new UnrecoverableException("");
        }

        String type = JsoupUtils.selectText("div.header_font", content);
        if (type == null) {
            logger.error("Unable to get form type");
            throw new UnrecoverableException("Unable to get form type");
        }

        ParsedTender tender;
        switch (type) {
            case "Tender Details":
                tender = CPPPNoticeHandler.parse(content);
                break;
            case "Award of Contract Details":
                tender = CPPPAwardHandler.parse(content);
                break;
            case "Corrigendum Details":
                tender = CPPPCorrigendumHandler.parse(content);
                break;
            default:
                logger.error("Unsupported form type {}", type);
                throw new UnrecoverableException("Unsupported form type");
        }

        tender.getPublications().get(0).setHumanReadableUrl(raw.getSourceUrl().toString());

        return Collections.singletonList(tender);
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }
}
