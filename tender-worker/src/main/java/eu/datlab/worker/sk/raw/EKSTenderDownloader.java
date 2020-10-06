package eu.datlab.worker.sk.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import org.apache.commons.io.FilenameUtils;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Tender downloader for EKS.
 *
 * @author Tomas Mrazek
 */
public final class EKSTenderDownloader extends BaseDownloader<RawData> {
    private static final String VERSION = "1.0";

    private static final int BATCH_SIZE = 5000;

    private static final String SOURCE_URL = "https://www.eks.sk";

    private static final String METADATA_HEADER_KEY = "header";

    @Override
    public List<RawData> downloadAndPopulateRawData(final Message message) {
        final List<RawData> rawData = new ArrayList<>();
        final String fileUrl = message.getValue("url");

        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(fileUrl));
            String line;
            int n = 0, batchSize = 0;
            StringBuilder batch = new StringBuilder();
            HashMap<String, Object> metaData = new HashMap<>();
            while ((line = reader.readLine()) != null) {
                n++;
                // skip header line
                if (n == 1) {
                    metaData.put(METADATA_HEADER_KEY, line.split("\\|"));
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
                raw.setMetaData(metaData);
                rawData.add(raw);

                batch = new StringBuilder();
                batchSize = 0;
            }

            if (batchSize > 0) {
                final RawData raw = new RawData();
                raw.setSourceData(batch.toString());
                raw.setSourceUrl(new URL(SOURCE_URL));
                raw.setSourceFileName(FilenameUtils.getName(fileUrl));
                raw.setMetaData(metaData);
                rawData.add(raw);
            }

            reader.close();
        } catch (IOException e) {
            logger.error("Unable to read file {}.", fileUrl, e);
            throw new UnrecoverableException("Unable to read csv.", e);
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
