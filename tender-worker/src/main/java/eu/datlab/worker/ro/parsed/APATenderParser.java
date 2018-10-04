package eu.datlab.worker.ro.parsed;

import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

/**
 * Parser for Romanian tenders.
 */
public class APATenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1";

    @Override
    public final List<ParsedTender> parse(final RawData raw) {
        final String firstLine = raw.getSourceData().split("\n", 2)[0];
        final int headerNumberOfValues = StringUtils.countMatches(firstLine, ",");

        if (headerNumberOfValues == 0) {
            return APATenderFormOneHandler.parse(raw, logger);
        } else {
            return APATenderFormTwoHandler.parse(raw, logger);
        }
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
