package eu.datlab.worker.cz.raw;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.compress.archivers.ArchiveStreamFactory;
import org.apache.commons.io.FilenameUtils;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import eu.dl.worker.utils.StringUtils;
import eu.dl.worker.utils.archive.ArchiveUtils;


/**
 * Downloads budgets data from Statni Pokladna and stores them into database.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaBudgetDownloader extends BaseDownloader<RawData> {

    private static final String VERSION = "1.0";
    
    private static final int CHUNK_LINES_COUNT = 5000;
    
    /**
     * List of regular expressions that match names of ignored files.
     */
    private static final List<String> BLACKLIST = Arrays.asList("^FINM204.*", "^FINU103.*");

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

            logger.info("Unpacking daily package {}.", fileUrl);
            final InputStream budgetsPackageStream = url.openStream();
            final HashMap<String, String> files = ArchiveUtils.extract(budgetsPackageStream,
                    FilenameUtils.getName(fileUrl), ArchiveStreamFactory.ZIP, null);

            for (final Map.Entry<String, String> file : files.entrySet()) {
                logger.info("Extracting file {}.", file.getKey());

                if (isOnBlacklist(file.getKey())) {
                    continue;
                }

                try {
                    StringUtils.chunkStringByLines(file.getValue(), CHUNK_LINES_COUNT, 1)
                        .forEach((csv) -> {
                            final RawData budgets = new RawData();
                            budgets.setSourceData(csv);
                            budgets.setSourceUrl(url);
                            budgets.setSourceFileName(file.getKey());

                            rawData.add(budgets);
                        });


                    logger.info("File {} processed.", file.getKey());
                } catch(IOException e) {
                    logger.error("Spliting of the file {} failed because of", file.getKey(), e);
                    throw new UnrecoverableException("Spliting of the file failed", e);
                }

                logger.info("New budgets downloaded from url {}.", fileUrl);
            }            
        } catch (final Exception e) {
            logger.error("Downloading failed for budget dataset {}.", fileUrl, e);
            throw new UnrecoverableException("Budget dataset downloading failed.", e);
        }

        return rawData;
    }

    /**
     * Checks whether the given file is on blacklist.
     *
     * @param file
     *      file name
     * @return true if file is on blacklist, otherwise false
     */
    private boolean isOnBlacklist(final String file) {
        return BLACKLIST.stream().anyMatch(regexp -> file.matches(regexp));
    }
    
    @Override
    public RawDAO<RawData> getRawDataDao() {
        return DAOFactory.getDAOFactory()
                .getRawBudgetItemDAO(getName(), getVersion());
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
