package eu.datlab.worker.at.parsed;

import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Parser for data.gv.at tenders.
 *
 * @author Miroslav Brezik
 */
public final class DataGvTenderParser extends BaseDatlabTenderParser {

    private static final String VERSION = "1.0";
    private static final String SOURCE = "https://www.data.gv.at";
    private static final String BASE_FORM_TYPE = "COMPILED";

    @Override
    protected String countryOfOrigin(final ParsedTender parsedTender, final RawData raw) {
        return "AT";
    }

    @Override
    public List<ParsedTender> parse(final RawData rawTender) {
        final Document doc = Jsoup.parse(rawTender.getSourceData(), "", Parser.xmlParser());
        final Element form = doc.children().stream()
                .filter(e -> e.tagName().startsWith("KD_"))
                .findFirst()
                .orElse(null);

        if (form == null) {
            logger.warn("Unknown xml document schema");
            return Collections.emptyList();
        }

        final ParsedTender parsedTender = new ParsedTender();
        String dispatchDate = null;
        switch (form.tagName()) {
            case "KD_8_2_Z1":
                DataGvTenderKD82Z1Handler.parseTender(parsedTender, form);
                break;
            case "KD_8_1_Z2":
                DataGvTenderKD81Z2Handler.parseTender(parsedTender, form);
                dispatchDate = Optional.ofNullable(form.selectFirst("COMPLEMENTARY_INFO > DATE_DISPATCH_NOTICE"))
                        .map(Element::text)
                        .orElse(null);
                break;
            case "KD_8_1_Z5":
                DataGvTenderKD81Z5Handler.parseTender(parsedTender, form);
                break;
            case "KD_8_2_Z3":
                DataGvTenderKD82Z3Handler.parseTender(parsedTender, form);
                break;
            case "KD_8_1_Z1":
                DataGvTenderKD81Z1Handler.parseTender(parsedTender, form);
                dispatchDate = Optional.ofNullable(form.selectFirst("COMPLEMENTARY_INFO > DATE_DISPATCH_NOTICE"))
                        .map(Element::text)
                        .orElse(null);
                break;
            case "KD_8_1_Z4":
                DataGvTenderKD81Z4Handler.parseTender(parsedTender, form);
                dispatchDate = Optional.ofNullable(form.selectFirst("COMPLEMENTARY_INFO > DATE_DISPATCH_NOTICE"))
                        .map(Element::text)
                        .orElse(null);
                break;
            default:
                logger.warn("Skipping parsing because of unsupported schema {}", form.tagName());
                return Collections.emptyList();
        }

        parsedTender
                .setPublications(Arrays.asList(
                        new ParsedPublication()
                            .setIsIncluded(true)
                            .setSourceTenderId(form.selectFirst("OBJECT_CONTRACT > REFERENCE_NUMBER").text())
                            .setSource(SOURCE)
                            .setMachineReadableUrl(rawTender.getSourceUrl().toString())
                            .setPublicationDate((String) rawTender.getMetaData().get("lastmod"))
                            .setSourceFormType(form.tagName())));
        Optional.ofNullable(dispatchDate).ifPresent(parsedTender.getPublications().get(0)::setDispatchDate);

        return Arrays.asList(parsedTender);
    }

    @Override
    protected List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }


}
