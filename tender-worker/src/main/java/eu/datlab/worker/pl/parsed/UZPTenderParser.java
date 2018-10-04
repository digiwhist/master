package eu.datlab.worker.pl.parsed;

import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import java.util.List;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Poland xml parsed.
 *
 * @author Tomas Mrazek
 */
public class UZPTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.1";

    private static final Logger logger = LoggerFactory.getLogger(UZPTenderParser.class);

    /**
     * Parses the given raw tender object.
     *
     * @param rawTender
     *         raw tender to be parsed
     *
     * @return list of parsed tenders or empty list if none tenders have been parsed
     */
    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        final Document document = Jsoup.parse(rawTender.getSourceData(), "", Parser.xmlParser());

        String machineReadableUrl = rawTender.getSourceUrl() + "?" + rawTender.getSourceFileName();

        final String formType = getFormType(document);
        switch (formType) {
            //Contract notice
            case "ZP-400":
            case "ZP-401":
            case "ZP-402":
            case "ZP-404":
            case "ZP-407":
            case "ZP-409":
                return UZPContractNoticeHandler.parse(document, machineReadableUrl);
            //Contract award notice
            case "ZP-403":
            case "ZP-405":
            case "ZP-408":
                return UZPContractAwardNoticeHandler.parse(document, machineReadableUrl);
            //Modification
            case "ZP-SPR":
            case "ZP-406":
                return UZPModificationHandler.parse(document, machineReadableUrl);
            default:
                return UZPDefaultHandler.parse(document, machineReadableUrl);
        }
    }

    /**
     * Returns actual version of this parsed manager.
     *
     * @return actual parsed manager version
     */
    @Override
    public final String getVersion() {
        return VERSION;
    }

    /**
     * Gets type of form for parsed document.
     *
     * @param context
     *         context
     *
     * @return type of form
     */
    private static String getFormType(final Element context) {
        return JsoupUtils.getRoot(context).tagName().toUpperCase();
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "PL";
    }
}
