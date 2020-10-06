package eu.datlab.worker.eu.raw;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.io.IOUtils;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import eu.dl.worker.utils.StringUtils;
import static org.apache.commons.codec.digest.DigestUtils.sha256Hex;

/**
 * Downloads TED tenders CSV. CSV files include many records, therefore the records are published in groups.
 *
 * @author Tomas Mrazek
 */
public final class TedCSVTenderDownloader extends BaseDownloader<RawData> {

    private static final String VERSION = "1.0";
    
    private static final int CHUNK_LINES_COUNT = 5000;
    
    private static final String PERSISTENT_ID_PREFIX = "EU";

    @Override
    protected boolean skipExisting(final Message message) {
        return false;
    }
    
    @Override
    public List<RawData> downloadAndPopulateRawData(final Message message) {
        final List<RawData> rawData = new ArrayList<>();
        final String fileUrl = message.getValue("url");

        try {
            final URL url = new URL(fileUrl);

            logger.info("Downloading CSV {}.", fileUrl);
            
            URLConnection con = url.openConnection();
            InputStream in = con.getInputStream();
            String encoding = con.getContentEncoding();
            encoding = encoding == null ? "UTF-8" : encoding;
            String csv = IOUtils.toString(in, encoding);

            // csv header - is necessary to send header, it will be used for column mapping because column order
            // change over time.
            String[] csvHeader = StringUtils.head(csv.replace("\"", ""), 1).split(",");
            HashMap<String, Object> metaData = new HashMap<>();
            metaData.put("csvHeader", csvHeader);
            
            try {
                List<String> chunks = StringUtils.chunkStringByLines(csv, CHUNK_LINES_COUNT, 1);

                for (int i = 0; i < chunks.size(); i++) {
                    int chunkNumber = i + 1;

                    final RawData budgets = new RawData();
                    budgets.setSourceData(chunks.get(i));
                    budgets.setSourceUrl(url);
                    budgets.setPersistentId(PERSISTENT_ID_PREFIX + "_" + sha256Hex(url.toString() + chunkNumber));
                    
                    metaData.put("chunkNumber", chunkNumber);
                    budgets.setMetaData(metaData);

                    rawData.add(budgets);
                }
            } catch(IOException e) {
                logger.error("Spliting of the file {} failed because of", fileUrl, e);
                throw new UnrecoverableException("Spliting of the file failed", e);
            }

            logger.info("New tenders from url {}.", fileUrl);
        } catch (final IOException e) {
            logger.error("Downloading failed for tenders CSV {}.", fileUrl, e);
            throw new UnrecoverableException("Budget dataset downloading failed.", e);
        }


        return rawData;
    }

    @Override
    public RawDAO<RawData> getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
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
