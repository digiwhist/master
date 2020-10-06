package eu.datlab.worker.cz.parsed;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.datlab.worker.cz.utils.StatniPokladnaBlacklist;
import eu.datlab.worker.cz.utils.StatniPokladnaBudgetUtils;
import eu.datlab.worker.parser.BaseDatlabBudgetItemParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.StringUtils;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is responsible for managing parsing of the raw budgets downloaded from Statni Pokladna.
 */
public final class StatniPokladnaBudgetParser extends BaseDatlabBudgetItemParser {
    private static final String VERSION = "1.0";

    private static final Logger logger = LoggerFactory.getLogger(StatniPokladnaBudgetParser.class);

    /**
     * Initialization of everything.
     */
    public StatniPokladnaBudgetParser() {
        super();
        config.addConfigFile("statni_pokladna");
    }



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

            final StatniPokladnaBlacklist blacklist = StatniPokladnaBlacklist.getInstance();

            final String filterBlacklist = config.getParam("StatniPokladnaBudget.filter.blacklist");
            final Set<String> filterReportTypes = config.getParamValueAsList("StatniPokladnaBudget.filter.reportTypes", ",", HashSet.class);

            return records.stream()
                .filter(r -> {
                    String ico = StringUtils.justifyLeft(r.get(handler.getIcoColumnIndex()), 8, "0");

                    if (ico != null) {
                        // ico is on blacklist
                        if ((filterBlacklist.equals("1") && blacklist.isBlack(ico))
                        // ico isn't on whitelist
                        || (filterBlacklist.equals("-1") && !blacklist.isBlack(ico))) {
                            return false;
                        }
                    }

                    if (filterReportTypes != null && !filterReportTypes.isEmpty()
                        && !filterReportTypes.contains(handler.getReportType().name())) {
                        return false;
                    }

                    return true;
                })
                .map(r -> handler.parse(r))
                .map(r -> r.setReport(handler.getReportType().name()))
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

    @Override
    protected List<ParsedBudgetItem> postProcessSourceSpecificRules(final List<ParsedBudgetItem> parsed, final RawData raw) {
        return parsed;
    }
}
