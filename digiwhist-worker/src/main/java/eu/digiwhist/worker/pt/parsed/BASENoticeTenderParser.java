package eu.digiwhist.worker.pt.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.clean.utils.DateUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Contract notice parser for Portugal.
 *
 * @author Tomas Mrazek
 */
public final class BASENoticeTenderParser extends BaseDigiwhistTenderParser {
    private static final String VERSION = "1.1";

    @Override
    public List<ParsedTender> parse(final RawData raw) {
        final HashMap<String, Object> metadata = raw.getMetaData();
        final Document doc = Jsoup.parse(raw.getSourceData());
        final Element table = JsoupUtils.selectFirst("table", doc);

        if (table == null) {
            return Collections.emptyList();
        }

        final String publicationDate =
            parsePublicationDate(JsoupUtils.getFirstLabeledValueNode(table, "Diário da República"));

        final ParsedTender tender = new ParsedTender()
            .addPublication(new ParsedPublication()
                .setIsIncluded(true)
                .setSourceFormType(BASETenderParserUtils.getFirstValueByLabel(table, "Tipo de ato"))
                .setSourceId(BASETenderParserUtils.getFirstValueByLabel(table, "Número do Anúncio"))
                .setHumanReadableUrl(raw.getSourceUrl().toString())
                .setSource(PublicationSources.PT_BASE)
                .setPublicationDate(publicationDate))
            .setSupplyType(BASETenderParserUtils.getFirstValueByLabel(table, "Tipo de contrato"))
            .setNationalProcedureType(BASETenderParserUtils.getFirstValueByLabel(table, "Modelo de anúncio"))
            .addBuyer(BASETenderParserUtils.parseBody(
                JsoupUtils.getFirstLabeledValueNode(table, "Entidade emissora - Nome, NIF")))
            .setDescription(BASETenderParserUtils.getFirstValueByLabel(table, "Descrição"))
            .setEstimatedPrice(BASETenderParserUtils.parsePrice(
                JsoupUtils.getFirstLabeledValueNode(table, "Preço base")))
            .setCpvs(BASETenderParserUtils.parseCPVs(JsoupUtils.getFirstLabeledValueNode(table, "CPV")))
            .setBidDeadline(parseBidDeadline(BASETenderParserUtils.getFirstValueByLabel(table,
                "Prazo para apresentação de propostas"), publicationDate));

        // associated contracts parsing
        if (metadata != null && metadata.containsKey("additionalUrls")) {
            HashMap<String, String> additionalUrls = (HashMap<String, String>) metadata.get("additionalUrls");

            for (Map.Entry<String, String> item : additionalUrls.entrySet()) {
                List<ParsedPublication> associatedContracts = parseAssociatedContracts(item.getValue());
                if (associatedContracts != null) {
                    tender.addPublications(associatedContracts);
                }
            }
        }

        return Collections.singletonList(tender);
    }

    /**
     * Parses bid deadline as {@code publicationDate} plus the given number of days or months. It uses ChronoUnit.DAYS
     * in case that {@code deadline} includes string "dias" and ChronoUnit.MONTHS for string "meses".
     * 
     * @param deadline
     *      string that includes information deadline
     * @param publicationDate
     *      publication date
     * @return deadline date string or null
     */
    private String parseBidDeadline(final String deadline, final String publicationDate) {
        LocalDate date = DateUtils.cleanDate(publicationDate, DateTimeFormatter.ofPattern("dd-MM-uuuu"));
        if (date == null || deadline == null) {
            return null;
        }

        Matcher m = Pattern.compile("([0-9]+) (dias|meses).*").matcher(deadline);
        if (m.find()) {
            ChronoUnit unit = m.group(2).equals("dias") ? ChronoUnit.DAYS : ChronoUnit.MONTHS;
            int count = Integer.parseInt(m.group(1));

            return date.plus(count, unit).toString();
        }

        return null;
    }

    /**
     * Parses list of associated contracts from the given {@code html}.
     *
     * @param html
     *      html of the page that includes list of associated contracts
     * @return non-empty list of prased publications or null
     */
    private List<ParsedPublication> parseAssociatedContracts(final String html) {
        Document doc = Jsoup.parse(html);

        Elements contracts = JsoupUtils.select("table#resultadosContractos > tbody > tr", doc);

        List<ParsedPublication> publications = new ArrayList<>();
        contracts.stream().forEach(row -> {
            publications.add(new ParsedPublication()
                .setIsIncluded(false)
                .setPublicationDate(BASETenderParserUtils.selectText("td:eq(2)", row))
                .setSource(PublicationSources.PT_BASE)
                .setHumanReadableUrl(JsoupUtils.selectAttribute("td:last-of-type a", "href", row)));
        });

        return publications.isEmpty() ? null : publications;
    }

    /**
     * Parses date from the given {@code dateNode}.
     *
     * @param dateNode
     *      node that includes date data
     * @return date string or null
     */
    private String parsePublicationDate(final Element dateNode) {
        if (dateNode == null) {
            return null;
        }

        Matcher m = Pattern.compile("[0-9]{2}\\-[0-9]{2}\\-[0-9]{4}").matcher(dateNode.text());
        return m.find() ? m.group() : null;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "PT";
    }
}
