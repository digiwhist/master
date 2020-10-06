package eu.datlab.worker.es.parsed;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.datlab.worker.parser.BaseDatlabBudgetItemParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.commons.csv.CSVRecord;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Spanish csv parser.
 */
public class BudgetCsvParser extends BaseDatlabBudgetItemParser {
    protected static final String VERSION = "1";

    private static final String ORGANICA = "Orgánica";
    private static final String EXPLICACION = "Explicación";
    static final String CAP1 = "Cap.1";
    static final String CAP2 = "Cap.2";
    static final String CAP3 = "Cap.3";
    static final String CAP4 = "Cap.4";
    private static final String CAP8 = "Cap.8";
    private static final String CAP9 = "Cap.9";
    static final String TOTAL = "Total";
    static final String[] CSV_TYPE_1 = new String[]{ORGANICA, EXPLICACION, CAP1, CAP2, CAP3};
    static final String[] CSV_TYPE_2 = new String[]{ORGANICA, EXPLICACION, CAP1, CAP3, CAP4};
    static final String[] CSV_TYPE_3 = new String[]{ORGANICA, EXPLICACION, CAP8, CAP9, TOTAL};
    static final String[] CSV_TYPE_4 = new String[]{ORGANICA, EXPLICACION, "", "", TOTAL};


    @Override
    public final List<ParsedBudgetItem> parse(final RawData raw) {
        try {
            final String header = raw.getSourceData().split("\n", 2)[0];
            final String year = "20" + raw.getSourceFileName().replaceAll(".*L_", "").replaceAll("_E.*", "");

            if (header.startsWith("Orgánica,Explicación,Cap.1,Cap.2,Cap.3") || header.startsWith(
                    "Orgánica,Explicación,Cap. 1,Cap. 2,Cap. 3")) {
                return BudgetCsvHandler1.parse(raw, year);
            } else if (header.startsWith("Orgánica,Explicación,Cap.1,Cap.3,Cap.4")) {
                return BudgetCsvHandler2.parse(raw, year);
            } else if (header.startsWith("Orgánica,Explicación,,,Total")) {
                return BudgetCsvHandler3.parse(raw, year);
            } else if (header.startsWith("Orgánica,Explicación,Cap.1,Cap.2,Cap.3")) {
                return BudgetCsvHandler4.parse(raw, year);
            } else {
                // no data to be parsed
                return new ArrayList<>();
            }
        } catch (IOException e) {
            logger.error("Unable to parse CSV: {}", e);
            throw new UnrecoverableException("Unable to parse CSV: ", e);
        }
    }

    @Override
    protected final List<ParsedBudgetItem> postProcess(final List<ParsedBudgetItem> parsed, final RawData raw) {
        return parsed;
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    /**
     * Parse one budget.
     *
     * @param csvRecord     csv record to parse budget from
     * @param valueSelector selector by which to find value
     * @param year          year of budget
     * @param source        where is budget parsed from
     *
     * @return ParsedBudgetItem
     */
    public static ParsedBudgetItem parseBudget(final CSVRecord csvRecord, final String valueSelector, final String
            year, final String source) {
        return new ParsedBudgetItem()
                .setBody(new ParsedBody()
                        .setName(csvRecord.get(EXPLICACION)))
                .setCountry("ESP")
                .setYear(year)
                .setLevel1Name(valueSelector)
                .setValue(csvRecord.get(valueSelector))
                .setSource(source);
    }

    @Override
    protected final List<ParsedBudgetItem> postProcessSourceSpecificRules(final List<ParsedBudgetItem> parsed, final RawData raw) {
        return parsed;
    }
}
