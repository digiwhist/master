package eu.datlab.worker.py.parsed;

import com.fasterxml.jackson.databind.JsonNode;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.py.DNCPFormType;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedCorrigendum;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.parsed.ParsedUnitPrice;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static eu.datlab.worker.py.parsed.DNCPParserUtils.parseIncludedPublication;
import static eu.datlab.worker.py.parsed.DNCPParserUtils.parseItems;
import static eu.datlab.worker.py.parsed.DNCPParserUtils.parseUnitPrice;
import static eu.datlab.worker.py.parsed.DNCPParserUtils.path;
import static eu.datlab.worker.py.parsed.DNCPParserUtils.textValue;

/**
 * DNCP Contract handler.
 *
 * @author Tomas Mrazek
 */
public final class DNCPContractHandler {

    private static final Logger logger = LoggerFactory.getLogger(DNCPContractHandler.class.getName());

    /**
     * Suppress default constructor.
     */
    private DNCPContractHandler() {
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
        if (path("releases/0/contracts", json).size() > 1) {
            logger.warn("List of contracts includes more than one element, parse the first one.");
        }

        JsonNode data = path("releases/0/contracts/0", json);

        Map<String, JsonNode> items = parseItems(data);

        StringBuilder desc = new StringBuilder();
        List<ParsedCPV> cpvs = new ArrayList<>();
        List<ParsedUnitPrice> unitPrices = new ArrayList<>();

        items.values().forEach(n -> {
            String cpvId = textValue("classification/id", n);

            desc.append(cpvId).append(": ").append(textValue("description", n)).append("\n\n");

            cpvs.add(new ParsedCPV().setCode(cpvId));

            unitPrices.add(parseUnitPrice(n));
        });

        String procedureType = (String) metaData.get("tipo_procedimiento");

        ParsedTender t = new ParsedTender()
            .addPublication(parseIncludedPublication(DNCPFormType.CONTRACT.name(), publicationDate, url, data))
            .addPublication(new ParsedPublication()
                .setSource(PublicationSources.PY_DNCP)
                .setIsIncluded(false)
                .setSourceId(textValue("awardID", data))
                .setSourceFormType(DNCPFormType.AWARD.name()))
            .setContractSignatureDate(textValue("dateSigned", data))
            .setDescription(desc.length() > 0 ? desc.toString().trim() : null)
            .setNationalProcedureType(procedureType)
            .setProcedureType(procedureType)
            .addLot(new ParsedTenderLot()
                .setCpvs(cpvs)
                .setContractNumber(textValue("id",  data))
                .addBid(new ParsedBid()
                    .addBidder(DNCPParserUtils.parseBody(DNCPParserUtils.path("suppliers", data)))
                    .setPrice(DNCPParserUtils.parsePrice(path("value", data)))
                    .setUnitPrices(unitPrices))
                .setTitle(textValue("title", data))
                .setEstimatedStartDate(textValue("period/startDate", data))
                .setEstimatedCompletionDate(textValue("period/endDate", data)))
            .setDocuments(DNCPParserUtils.parseDocuments(path("documents", data)));

        String replacementDate = (String) metaData.get("vigencia_contrato");
        if (replacementDate != null) {
            t.addCorrigendum(new ParsedCorrigendum().setReplacementDate(replacementDate));
        }

        return Arrays.asList(t);
    }
}
