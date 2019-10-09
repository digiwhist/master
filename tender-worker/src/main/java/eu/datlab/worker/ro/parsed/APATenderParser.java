package eu.datlab.worker.ro.parsed;

import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;

import java.util.List;

/**
 * Parser for Romanian tenders.
 */
public class APATenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1";

    @Override
    public final List<ParsedTender> parse(final RawData raw) {

        if(raw.getSourceData() == null && raw.getSourceBinaryData() != null){
            return APATenderExcelHandler.parse(raw, logger);
        } else if(raw.getSourceData() != null && raw.getSourceBinaryData() == null){
            return APATenderCsvHandler.parse(raw, logger);
        }
        return null;
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return CountryCode.RO.toString();
    }
}
