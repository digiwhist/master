package eu.digiwhist.worker.hr.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Tender parser for Croatia.
 *
 * @author Marek Mikes
 */
public class EOJNTenderParser extends BaseDigiwhistTenderParser {
    private static final String VERSION = "1";

    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        if (rawTender.getSourceData().isEmpty()) {
            logger.warn("Raw data are empty! Downloader probably downloaded the form incorrectly.");
            return Collections.singletonList(new ParsedTender()
                    .addPublication(new ParsedPublication()
                            .setIsIncluded(true)
                            .setHumanReadableUrl(rawTender.getSourceUrl().toString())
                            .setSource(PublicationSources.HR_EOJN)));
        }

        final Document document = Jsoup.parse(rawTender.getSourceData());

        switch (EOJNTenderFormUtils.getFormAge(document)) {
            case ANCIENT:
                return Arrays.asList(EOJNTenderAncientHandler.parse(document, rawTender.getSourceUrl().toString()));
            case OLD:
                return Arrays.asList(EOJNTenderOldHandler.parse(document, rawTender.getSourceUrl().toString()));
            case NEW:
                return Arrays.asList(EOJNTenderNewHandler.parse(document, rawTender.getSourceUrl().toString()));
            default:
                assert false;
                throw new UnrecoverableException("Probably some age was added and we do not have ant handler.");
        }
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "HR";
    }
}
