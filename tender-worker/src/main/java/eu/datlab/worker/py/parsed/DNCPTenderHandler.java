package eu.datlab.worker.py.parsed;

import com.fasterxml.jackson.databind.JsonNode;
import eu.datlab.worker.py.DNCPFormType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static eu.datlab.worker.py.parsed.DNCPParserUtils.parseDeadline;
import static eu.datlab.worker.py.parsed.DNCPParserUtils.parseIncludedPublication;
import static eu.datlab.worker.py.parsed.DNCPParserUtils.path;
import static eu.datlab.worker.py.parsed.DNCPParserUtils.textValue;

/**
 * DNCP Tender handler.
 *
 * @author Tomas Mrazek
 */
public final class DNCPTenderHandler {

    /**
     * Suppress default constructor.
     */
    private DNCPTenderHandler() {
    }

    /**
     * @param json
     *      JSON to be parsed
     * @param publicationDate
     *      publication date
     * @param url
     *      human readable url
     * @param metaData
     *      raw message metadata
     * @return list of parsed tenders
     */
    public static List<ParsedTender> parse(final JsonNode json, final String publicationDate, final String url,
                                           final Map<String, Object> metaData) {
        JsonNode data = path("releases/0/tender", json);

        ParsedTender t = new ParsedTender()
            .addPublication(parseIncludedPublication(DNCPFormType.TENDER.name(), publicationDate, url, data))
            .setTitle(textValue("title", data))
            .setDocuments(DNCPParserUtils.parseDocuments(path("documents", data)))
            .setEnquiryDeadline(parseDeadline("enquiryPeriod", data))
            .setBidDeadline(parseDeadline("tenderPeriod", data))
            .setAwardDeadline(parseDeadline("awardPeriod", data))
            .setEstimatedPrice(DNCPParserUtils.parsePrice(path("value", data)))
            .addBuyer(DNCPParserUtils.parseBody(path("procuringEntity", data)));

        String funding = (String) metaData.get("fuente_financiamiento");
        if (funding != null) {
            t.addFunding(new ParsedFunding().setSource(funding));
        }

        String bidsRecipientAddress = (String) metaData.get("lugar_entrega_oferta");
        if (bidsRecipientAddress != null) {
            t.setBidsRecipient(new ParsedBody().setAddress(new ParsedAddress().setRawAddress(bidsRecipientAddress)));
        }

        return Arrays.asList(t);
    }
}
