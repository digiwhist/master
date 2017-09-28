package eu.digiwhist.worker.cz.parsed;

import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.digiwhist.worker.cz.utils.StatniPokladnaBudgetUtils;
import eu.digiwhist.worker.parser.BaseDigiwhistBudgetItemParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.raw.RawData;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is responsible for managing parsing of the raw budgets downloaded from Statni Pokladna.
 */
public final class StatniPokladnaBudgetParser extends BaseDigiwhistBudgetItemParser {
    private static final String VERSION = "1.0";

    private static final Logger logger = LoggerFactory.getLogger(StatniPokladnaBudgetParser.class);

    /**
     * Parses the given raw budget object.
     *
     * @param rawBudget
     *         raw budget to be parsed
     *
     * @return list of prased budget items
     * @throws UnrecoverableException
     *         if no parsed could be created for given raw tender
     */
    @Override
    public List<ParsedBudgetItem> parse(final RawData rawBudget) {
        logger.info("Trying to parse csv {} from {}.", rawBudget.getSourceFileName(), rawBudget.getSourceUrl());

        final StatniPokladnaHandler handler = StatniPokladnaBudgetUtils.getHandler(rawBudget.getSourceFileName());

        try {
            final CSVParser parser = CSVParser.parse(rawBudget.getSourceData(), CSVFormat.RFC4180.withDelimiter(';'));
            final List<CSVRecord> records = parser.getRecords();

            return records.stream()
                .map(r -> handler.parse(r))
                .collect(Collectors.toList());
        } catch (IOException e) {
            logger.error("Unable to prase CSV {} because of exception", rawBudget.getSourceFileName(), e);
            throw new UnrecoverableException("Unable to prase CSV.", e);
        }
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected List<ParsedBudgetItem> postProcess(final List<ParsedBudgetItem> parsed, final RawData raw) {        
        parsed.stream().forEach((item) -> item
            .setCurrency("CZK")
            .setSource(raw.getSourceUrl().toString()));

        return parsed;
    }
}
