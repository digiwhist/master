package eu.datlab.worker.py.parsed;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.datlab.worker.py.DNCPFormType;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import java.io.IOException;
import java.util.HashMap;

import java.util.List;

/**
 * Tender parsed for Paraguay.
 *
 * @author Tomas Mrazek
 */
public class DNCPTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        HashMap metaData = rawTender.getMetaData();
        String formType = (String) metaData.get("formType");
        String publicationDate = (String) metaData.get("publicationDate");
        String url = rawTender.getSourceUrl().toString();

        JsonNode json = null;
        try {
            ObjectMapper mapper = new ObjectMapper();
            json = mapper.readTree(rawTender.getSourceData());
        } catch (IOException ex) {
            logger.error("Unable to parse JSON from raw data");
            throw new UnrecoverableException("Unable to parse JSON from raw data", ex);
        }

        switch (DNCPFormType.valueOf(formType)) {
            case AMENDMENT:
                return DNCPAmendmentHandler.parse(json, publicationDate, url, metaData);
            case AWARD:
                return DNCPAwardHandler.parse(json, publicationDate, url, metaData);
            case CONTRACT:
                return DNCPContractHandler.parse(json, publicationDate, url, metaData);
            case PLANNING:
                return DNCPPlanningHandler.parse(json, publicationDate, url, metaData);
            case TENDER:
                return DNCPTenderHandler.parse(json, publicationDate, url, metaData);
            default:
                logger.error("Unsupported DNCP form type {}", formType);
                throw new UnrecoverableException("Unsupported DNCP form type");
        }
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "PY";
    }

    @Override
    protected final List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }
}
