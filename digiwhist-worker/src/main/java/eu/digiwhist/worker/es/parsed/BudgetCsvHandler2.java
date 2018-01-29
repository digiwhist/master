package eu.digiwhist.worker.es.parsed;

import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static eu.digiwhist.worker.es.parsed.BudgetCsvParser.CAP1;
import static eu.digiwhist.worker.es.parsed.BudgetCsvParser.CAP3;
import static eu.digiwhist.worker.es.parsed.BudgetCsvParser.CAP4;
import static eu.digiwhist.worker.es.parsed.BudgetCsvParser.CSV_TYPE_2;
import static eu.digiwhist.worker.es.parsed.BudgetCsvParser.parseBudget;

/**
 * Budget csv parsing handler.
 */
public final class BudgetCsvHandler2 {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private BudgetCsvHandler2() {
    }

    /**
     * Parse data from CSV.
     *
     * @param raw raw data
     * @param year year of publication
     *
     * @return List<ParsedBudgetItem>
     * @throws IOException exception on faulty CSV
     */
    public static List<ParsedBudgetItem> parse(final RawData raw, final String year) throws IOException {
        final CSVParser csvParser = CSVParser.parse(raw.getSourceData(),
                CSVFormat.newFormat(',').withHeader(CSV_TYPE_2).withSkipHeaderRecord(true));
        final List<CSVRecord> budgets = csvParser.getRecords();

        final List<ParsedBudgetItem> parsedTenders = new ArrayList<>();

        for (int i = 0; i < budgets.size(); i++) {
            CSVRecord budget = budgets.get(i);

            try {
                if (budget.size() > 2) {
                    parsedTenders.add(parseBudget(budget, CAP1, year, raw.getSourceFileName() + ":" + i));
                }

                if (budget.size() > 3) {
                    parsedTenders.add(parseBudget(budget, CAP3, year, raw.getSourceFileName() + ":" + i));
                }

                if (budget.size() > 4) {
                    parsedTenders.add(parseBudget(budget, CAP4, year, raw.getSourceFileName() + ":" + i));
                }
            } catch (IllegalArgumentException e) {

            }
        }


        return parsedTenders;
    }
}
