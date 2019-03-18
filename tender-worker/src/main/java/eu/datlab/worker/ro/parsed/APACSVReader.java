package eu.datlab.worker.ro.parsed;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

/**
 * APA CSV reader which fixes number of columns in particular row.
 */
public class APACSVReader extends BufferedReader {
    private final Integer columns;
    private static final Character DELIMITER = '^';
    private final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    /**
     * Reader initialization.
     *
     * @param csv
     *      csv data
     * @param columns
     *      required number of columns
     */
    public APACSVReader(final String csv, final int columns) {
        super(new BufferedReader(new StringReader(csv)));
        this.columns = columns;
    }

    @Override
    public final String readLine() throws IOException {
        String line = super.readLine();

        if (line != null && !line.isEmpty()) {
            int matches = StringUtils.countMatches(line, DELIMITER) + 1;
            if (matches < columns) {
                logger.warn("Line '{}' doesn't include required number of columns.", line);
                line += StringUtils.repeat(DELIMITER, columns - matches);
            }
        }

        return line;
    }
}
