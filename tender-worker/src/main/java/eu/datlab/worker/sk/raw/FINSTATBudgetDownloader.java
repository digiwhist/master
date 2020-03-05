package eu.datlab.worker.sk.raw;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FilenameUtils;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;

/**
 * Publishes incoming file as a set of batches. Size of the batch is limited with a number of rows.
 *
 * @author Tomas Mrazek
 */
public final class FINSTATBudgetDownloader extends BaseDownloader<RawData> {

    private static final String VERSION = "1.0";
    
    private static final int BATCH_SIZE = 5000;

    private static final String SOURCE_URL = "https://www.finstat.sk";

    @Override
    public List<RawData> downloadAndPopulateRawData(final Message message) {
        final List<RawData> rawData = new ArrayList<>();
        final String fileUrl = message.getValue("url");

        BufferedReader reader;
		try {
			reader = new BufferedReader(new FileReader(fileUrl));
            String line;
            int batchSize = 0;
            StringBuilder batch = new StringBuilder();
			while ((line = reader.readLine()) != null) {
                // skip header lines
                if (line.startsWith("Ico;")) {
                    continue;
                }

                batch.append(line).append("\n");
                batchSize++;
                if (batchSize < BATCH_SIZE) {
                    continue;
                }

                final RawData raw = new RawData();
                raw.setSourceData(batch.toString());
                raw.setSourceUrl(new URL(SOURCE_URL));
                raw.setSourceFileName(FilenameUtils.getName(fileUrl));
                rawData.add(raw);

                batch = new StringBuilder();
                batchSize = 0;
            }
            
            if (batchSize > 0) {
                final RawData raw = new RawData();
                raw.setSourceData(batch.toString());
                raw.setSourceUrl(new URL(SOURCE_URL));
                raw.setSourceFileName(FilenameUtils.getName(fileUrl));
                rawData.add(raw);
            }

			reader.close();
		} catch (IOException e) {
			logger.error("Unable to read file {}.", fileUrl, e);
            throw new UnrecoverableException("Unable to read budget dataset.", e);
		}

        return rawData;
    }

    @Override
    public RawDAO<RawData> getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawBudgetItemDAO(getName(), getVersion());
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @Override
    protected void postProcess(final RawData raw) {
    }
}
