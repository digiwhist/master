package eu.datlab.worker.uk.parsed;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.datlab.worker.parser.BaseDatlabBudgetItemParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.raw.RawData;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Parser for UK budgets.
 */
public class UKBudgetParser extends BaseDatlabBudgetItemParser {
    private static final String VERSION = "1";

    @Override
    public final List<ParsedBudgetItem> parse(final RawData raw) {
        final BufferedReader data = new BufferedReader(new StringReader(raw.getSourceData()));

        final List<ParsedBudgetItem> result = new ArrayList<>();

        HashMap<String, Object> metadata = raw.getMetaData();
        String year = metadata != null ? (String) metadata.get("year") : null;
        if (year == null) {
            logger.error("Metadata of the raw record {} doesn't include key 'year'", raw.getId());
            throw new UnrecoverableException("Metadata of the raw record {} doesn't include key 'year'");
        }

        String line;
        try {
            boolean budgetsStarted = false;
            String currentlyParsedBody = null;
            String previousLine = null;

            while ((line = data.readLine()) != null/* && !line.contains("Section 6")*/) {
                String[] possibleBudget = line.split(" ");

                if (line.contains("Supply Estimates presented by HM Treasury")) {
                    budgetsStarted = true;
                } else if (budgetsStarted && currentlyParsedBody != null && possibleBudget.length == 4) {
                    for (int i = 1; i <= 3; i++) {
                        result.add(new ParsedBudgetItem()
                                .setYear(year)
                                .setBody(new ParsedBody()
                                        .setName(currentlyParsedBody))
                                .setCurrency("GBP")
                                .setCountry("GBR")
                                .setSource(raw.getSourceUrl().toString())
                                .setLevel2Name(possibleBudget[0])
                                .setValue(possibleBudget[i]));
                    }
                } else if (previousLine != null && previousLine.trim().isEmpty() && !line
                        .equalsIgnoreCase("Departmental Expenditure Limit")) {
                    previousLine = line;
                    currentlyParsedBody = line;
                } else {
                    previousLine = line;
                }
            }
        } catch (IOException e) {

        }

        return result;
    }

    @Override
    protected final List<ParsedBudgetItem> postProcess(final List<ParsedBudgetItem> parsed, final RawData raw) {
        return parsed;
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }
}
