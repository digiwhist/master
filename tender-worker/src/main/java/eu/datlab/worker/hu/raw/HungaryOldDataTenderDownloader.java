package eu.datlab.worker.hu.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import org.apache.commons.lang3.ArrayUtils;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * This downloader imports Hungary data from years 2005-2012 from text file which is locally stored.
 * Each row (represents one tender) is save to raw data, so one record in raw data is one tender.
 *
 * @author Marek Mikes
 */
public class HungaryOldDataTenderDownloader extends BaseDownloader<RawData> {
    private static final String VERSION = "1";

    private static final int COLUMNS_COUNT = 573;

    @Override
    protected final boolean skipExisting(final Message message) {
        return false;
    }

    @Override
    public final RawDAO getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @Override
    public final List<RawData> downloadAndPopulateRawData(final Message message) {
        final List<RawData> rawData = new ArrayList<>();

        final String file = config.getParam(getName() + ".filePath");
        try (BufferedReader br = new BufferedReader(new FileReader(file))) {

            // load first row, which is header
            String line = br.readLine();
            assert line.split("\\t", -1).length == COLUMNS_COUNT;

            while ((line = br.readLine()) != null) {
                final RawData record = rawDao.getEmptyInstance();

                String[] columns = line.split("\\t", -1);
                if (columns.length != COLUMNS_COUNT) {
                    assert columns.length < COLUMNS_COUNT;
                    logger.warn("Loaded row has only " + String.valueOf(columns.length)
                            + " columns, because it contains new line character after \"" + line + "\".");

                    // get next line, which we want to join
                    line = br.readLine();
                    String[] nextRowColumns = line.split("\\t", -1);

                    // assert contains -1, because one column is in "columns" and "nextRowColumns"
                    assert columns.length + nextRowColumns.length - 1 == COLUMNS_COUNT;

                    // join the divided column
                    columns[columns.length - 1] = columns[columns.length - 1] + nextRowColumns[0];
                    nextRowColumns = Arrays.copyOfRange(nextRowColumns, 1, nextRowColumns.length);

                    // join the two arrays
                    columns = (String[]) ArrayUtils.addAll(columns, nextRowColumns);
                    assert columns.length == COLUMNS_COUNT;

                    // join array back to line
                    StringBuilder builder = new StringBuilder();
                    for (int i = 0; i < columns.length - 1; ++i) {
                        builder.append(columns[i]).append('\t');
                    }
                    builder.append(columns[columns.length - 1]);
                    line = builder.toString();
                }

                // set record because of persistent ID. Number at the end of file name represents row number
                record.setSourceUrl(new URL("file://" + file));
                record.setSourceFileName(file + ":" + String.valueOf(rawData.size() + 1));

                record.setSourceData(line);

                rawData.add(record);
            }
        } catch (IOException e) {
            logger.error("There was some problem during loading file. Exception message is \"{}\"", e.getMessage());
            return null;
        }

        return rawData;
    }

    @Override
    protected final void postProcess(final RawData raw) {
    }
}
