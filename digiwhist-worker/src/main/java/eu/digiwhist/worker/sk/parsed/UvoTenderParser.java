package eu.digiwhist.worker.sk.parsed;

import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;

import static eu.digiwhist.worker.sk.parsed.UvoTenderParserUtils.getFirstValueFromElement;

/**
 * Tender parsed for Slovakia.
 *
 * @author Michal Riha
 */
public class UvoTenderParser extends BaseDigiwhistTenderParser {
    private static final String VERSION = "5";

    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");
    private static final LocalDate FIRST_OLD_FORMS = LocalDate.of(2012, 10, 24);
    private static final LocalDate FIRST_NEW_FORMS = LocalDate.of(2016, 4, 18);

    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        final Document document = Jsoup.parse(rawTender.getSourceData());

        // Find out tender creation date
        String publicationDateElement =
                getFirstValueFromElement(document, "div.notice > div:containsOwn(Vestník č.)");

        if (publicationDateElement == null) {
            logger.error("Publication is in unknown format.");
            throw new UnrecoverableException("Publication is in unknown format.");
        }

        String publicationDate = publicationDateElement.split("-")[1].trim();
        LocalDate formPublicationDate = LocalDate.parse(publicationDate, formatter);

        // Pick correct parsed based on tenders age
        if (formPublicationDate.isBefore(FIRST_OLD_FORMS)) {
            return new UvoTenderAncientHandler().parse(document, rawTender.getSourceUrl().toString(), publicationDate);
        } else if (formPublicationDate.isBefore(FIRST_NEW_FORMS)) {
            return new UvoTenderOldHandler().parse(document, rawTender.getSourceUrl().toString(), publicationDate);
        } else {
            return new UvoTenderNewHandler().parse(document, rawTender.getSourceUrl().toString(), publicationDate);
        }
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "SK";
    }
}
