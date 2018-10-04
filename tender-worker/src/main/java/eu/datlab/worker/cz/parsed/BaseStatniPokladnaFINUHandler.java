package eu.datlab.worker.cz.parsed;

import eu.datlab.dataaccess.dto.codetables.BudgetItemReportType;
import org.apache.commons.csv.CSVRecord;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;

/**
 * Base handler class for parsers of FINU csv dataset.
 *
 * @author Tomas Mrazek
 */
public abstract class BaseStatniPokladnaFINUHandler implements StatniPokladnaHandler {

    @Override
    public final ParsedBudgetItem parse(final CSVRecord record) {
        return parseDatasetSpecificFields(StatniPokladnaBudgetParserUtils.parseCommonFINUBudgetItem(record, getIcoColumnIndex()), record);
    }

    /**
     * Parses FINU handler specific fields.
     *
     * @param parsed
     *      prased budget item with filled FINM common fields
     * @param record
     *      csv record
     * @return budget item updated witrh handler specific fields
     */
    protected abstract ParsedBudgetItem parseDatasetSpecificFields(ParsedBudgetItem parsed, CSVRecord record);

    @Override
    public final int getIcoColumnIndex() {
        return 4;
    }
    
    @Override
    public final BudgetItemReportType getReportType() {
        return BudgetItemReportType.BUDGET_EXECUTION;
    }
}
