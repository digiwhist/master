package eu.datlab.worker.fr.parsed;

import eu.datlab.worker.fr.BOAMPTenderUtils;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

import static eu.datlab.worker.fr.BOAMPTenderUtils.HTML_SOURCE_DATA_METADATA_KEY;
import static eu.datlab.worker.fr.BOAMPTenderUtils.HTML_SOURCE_URL_METADATA_KEY;

import java.util.HashMap;
import java.util.List;

/**
 * Tender parser for France. It parses publications from FTP server and its HTML versions.
 *
 * @author Marek Mikes
 */
public class BOAMPTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1";

    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        // parse the publication from the FTP
        final List<ParsedTender> parsedTenders = BOAMPTenderUtils.isNewXmlFormat(rawTender.getSourceFileName())
                ? BOAMPTenderNewHandler.parse(rawTender)
                : BOAMPTenderOldHandler.parse(rawTender);

        assert parsedTenders.size() == 1;

        // parse the publication from the web
        final HashMap<String, Object> metaData = rawTender.getMetaData();
        final Document document = Jsoup.parse((String) metaData.get(HTML_SOURCE_DATA_METADATA_KEY));
        final String humanReadableUrl = (String) metaData.get(HTML_SOURCE_URL_METADATA_KEY);
        BOAMPWebTenderHandler.parse(document, humanReadableUrl, parsedTenders.get(0));

        return parsedTenders;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "FR";
    }
}
