package eu.datlab.worker.sk.parsed;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.datlab.worker.parser.BaseDatlabBudgetItemParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static eu.datlab.dataaccess.dto.codetables.BudgetItemReportType.FINSTAT;

/**
 * Parses FINSTAT csv.
 */
public final class FINSTATBudgetParser extends BaseDatlabBudgetItemParser {
    private static final String VERSION = "1.0";

    private static final Logger logger = LoggerFactory.getLogger(FINSTATBudgetParser.class);

    private static final List<List<String>> cells = new ArrayList<>();
    static {
        // level3 code, level3 name, value
        cells.add(Arrays.asList("ODNM", "Obstaranie dlhodobého nehmotného majetku", "AH"));
        cells.add(Arrays.asList("ODHM", "Obstaranie dlhodobého hmotného majetku", "BH"));
        cells.add(Arrays.asList("501", "Spotreba materiálu", "NV"));
        cells.add(Arrays.asList("502", "Spotreba energie", "NX"));
        cells.add(Arrays.asList("503", "Spotreba ostatných neskladovateľných dodávok", "NZ"));
        cells.add(Arrays.asList("504", "Predaný tovar (náklady vynaložené na predanie tovaru)", "OB"));
        cells.add(Arrays.asList("511", "Opravy a udržiavanie", "OF"));
        cells.add(Arrays.asList("512", "Cestovné", "OH"));
        cells.add(Arrays.asList("513", "Náklady na reprezentáciu", "OJ"));
        cells.add(Arrays.asList("518", "Ostatné služby", "OL"));
        cells.add(Arrays.asList("548", "Ostatné náklady na prevádzkovú činnosť", "PT"));
    }

    /**
     * Parses the given raw budget object.
     *
     * @param rawBudget
     *         raw budget to be parsed
     *
     * @return list of parsed budget items
     */
    @Override
    public List<ParsedBudgetItem> parse(final RawData rawBudget) {
        logger.info("Trying to parse csv {} from {}.", rawBudget.getSourceFileName(), rawBudget.getSourceUrl());

        try {
            final CSVParser parser = CSVParser.parse(rawBudget.getSourceData(), CSVFormat.RFC4180.withDelimiter(';').withQuote('"'));
            final List<CSVRecord> records = parser.getRecords();

            List<ParsedBudgetItem> parsed = new ArrayList<>();
            for (CSVRecord r : records) {
                ParsedBody body = new ParsedBody()
                    .setName(r.get(cellAddrToIndex("E")))
                    .addBodyId(new BodyIdentifier()
                        .setId(r.get(cellAddrToIndex("A")))
                        .setScope(BodyIdentifier.Scope.SK)
                        .setType(BodyIdentifier.Type.ORGANIZATION_ID))
                    .setAddress(new ParsedAddress()
                        .setCity(r.get(cellAddrToIndex("I")))
                        .setStreet(r.get(cellAddrToIndex("H"))));

                for (List<String> c : cells) {
                    parsed.add(new ParsedBudgetItem()
                        .setReport(FINSTAT.toString()).setBody(body).setYear(r.get(cellAddrToIndex("B")))
                        .setLevel3Code(c.get(0)).setLevel3Name(c.get(1)).setValue(r.get(cellAddrToIndex(c.get(2))))
                    );
                }
            }

            return parsed;
        } catch (IOException e) {
            logger.error("Unable to parse CSV {} because of exception", rawBudget.getSourceFileName(), e);
            throw new UnrecoverableException("Unable to parse CSV.", e);
        }
    }

    /**
     * @param addr
     *      case insensitive address of cell (eg. BAH)
     *
     * @return 0-based index of column
     */
    private static int cellAddrToIndex(final String addr) {
        int exp = 0;
        int index = 0;
        byte[] bytes = addr.toUpperCase().getBytes();

        // reverse order
        for (int i = bytes.length - 1; i >= 0; i--) {
            // 65 = 'A'
            index += (bytes[i] - 64) * Math.pow(26, exp);
            exp++;
        }

        return index - 1;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected List<ParsedBudgetItem> postProcess(final List<ParsedBudgetItem> parsed, final RawData raw) {        
        parsed.stream().forEach((item) -> item
            .setCurrency("EUR")
            .setSource(raw.getSourceUrl().toString()));

        return parsed;
    }

    @Override
    protected List<ParsedBudgetItem> postProcessSourceSpecificRules(final List<ParsedBudgetItem> parsed, final RawData raw) {
        return parsed;
    }
}
