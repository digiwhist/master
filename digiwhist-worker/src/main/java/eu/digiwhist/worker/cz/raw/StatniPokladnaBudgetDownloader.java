package eu.digiwhist.worker.cz.raw;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.compress.archivers.ArchiveStreamFactory;
import org.apache.commons.io.FilenameUtils;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.digiwhist.worker.cz.parsed.StatniPokladnaHandler;
import eu.digiwhist.worker.cz.utils.StatniPokladnaBlacklist;
import eu.digiwhist.worker.cz.utils.StatniPokladnaBudgetUtils;
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
    public List<RawData> downloadAndPopulateRawData(final Message message) {
        final List<RawData> rawData = new ArrayList<>();
        final String fileUrl = message.getValue("url");
        final StatniPokladnaBlacklist blacklist = StatniPokladnaBlacklist.getInstance();

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
                    StatniPokladnaHandler handler = StatniPokladnaBudgetUtils.getHandler(file.getKey());

                    // position of the ICO depends on the handler
                    // sometimes the ICO has ten digits, where the first two digits are 0
                    final Pattern icoPattern = Pattern.compile("(?:[^;]*;){" + handler.getIcoColumnIndex() + "}"
                        + "(?<ico>[0-9]+);");

                    // The lambda function supports only final or efectively final variables from a context. This little
                    // trick allows us to change value of the final variable.
                    final int[] skipped = {0};

                    StringUtils.chunkStringByLines(file.getValue(), CHUNK_LINES_COUNT, 1, l -> {
                            Matcher m = icoPattern.matcher(l);                            
                            boolean isBlack = false;
                            if (m.find()) {
                                // normalize ICO to 8 characters length, if the ICO is shorter appends zeros on begin
                                isBlack = blacklist.isBlack(StringUtils.justifyLeft(m.group("ico"), 8, "0"));
                            }

                            if (isBlack) {
                                skipped[0]++;
                            }
                            
                            return !isBlack;
                        })
                        .forEach((csv) -> {
                            final RawData budgets = new RawData();
                            budgets.setSourceData(csv);
                            budgets.setSourceUrl(url);
                            budgets.setSourceFileName(file.getKey());

                            rawData.add(budgets);
                        });


                    logger.info("File {} processed. {} items out of total {} were skipped.", file.getKey(),
                        skipped[0], file.getValue().split("\n").length);
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
}
