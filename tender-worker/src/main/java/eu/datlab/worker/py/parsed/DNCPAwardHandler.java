package eu.datlab.worker.py.parsed;

import com.fasterxml.jackson.databind.JsonNode;
import eu.datlab.worker.py.DNCPFormType;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static eu.datlab.worker.py.parsed.DNCPParserUtils.parseIncludedPublication;
import static eu.datlab.worker.py.parsed.DNCPParserUtils.path;
import static eu.datlab.worker.py.parsed.DNCPParserUtils.textValue;

/**
 * DNCP Award handler.
 *
 * @author Tomas Mrazek
 */
public final class DNCPAwardHandler {
    private static final Logger logger = LoggerFactory.getLogger(DNCPAwardHandler.class.getName());

    /**
     * CSV parser.
     */
    private static final CSVFormat PARSER_FORMAT = CSVFormat.DEFAULT.withHeader().withSkipHeaderRecord().withDelimiter(';');

    /**
     * Suppress default constructor.
     */
    private DNCPAwardHandler() {
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
        JsonNode data = path("releases/0/awards/0", json);

        String procedureType = (String) metaData.get("tipo_procedimiento");

        // parsing of bidders CSV
        Map<String, String> biddersMetaData = (HashMap<String, String>) metaData.get("bidders");
        List<CSVRecord> bidders;
        if (biddersMetaData != null) {
            try {
                String biddersCSV = biddersMetaData.entrySet().iterator().next().getValue();
                CSVParser parser = CSVParser.parse(biddersCSV, PARSER_FORMAT);
                bidders = parser.getRecords();
                parser.close();
            } catch (final IOException ex) {
                logger.error("Unable to parse bidders meta-data {}", biddersMetaData, ex);
                throw new UnrecoverableException("Unable to parse bidders meta-data");
            }
        } else {
            bidders = Collections.emptyList();
        }

        ParsedTender t = new ParsedTender()
            .addPublication(parseIncludedPublication(DNCPFormType.AWARD.name(), publicationDate, url, data))
            .setTitle(textValue("title", data))
            .setFinalPrice(DNCPParserUtils.parsePrice(path("value", data)))
            .setNationalProcedureType(procedureType)
            // create separate lot for each supplier
            .setLots(DNCPParserUtils.parseList(path("suppliers", data).elements(),
                n -> {
                    ParsedTenderLot lot = new ParsedTenderLot().setContractNumber(textValue("identifier/key", n));

                    ParsedBody winner = DNCPParserUtils.parseBody(n);
                    String winnerName = textValue("identifier/legalName", n);

                    // winning bid
                    lot.addBid(new ParsedBid().setIsWinning(Boolean.TRUE.toString()).addBidder(winner));

                    // other bids
                    for (CSVRecord b : bidders) {
                        if (!b.get("razon_social").equals(winnerName)) {
                            lot.addBid(new ParsedBid().setIsWinning(Boolean.FALSE.toString())
                                .addBidder(new ParsedBody()
                                    .setName(b.get("razon_social"))
                                    .addBodyId(new BodyIdentifier().setId(b.get("ruc")))));
                        }
                    }

                    return lot;
                }));

        String funding = (String) metaData.get("organismo_financiador");
        if (funding != null) {
            t.addFunding(new ParsedFunding().setSource(funding));
        }

        return Arrays.asList(t);
    }
}
