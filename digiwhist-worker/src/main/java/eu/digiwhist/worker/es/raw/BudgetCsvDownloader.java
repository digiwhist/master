package eu.digiwhist.worker.es.raw;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * Spanish budget downloader.
 */
public class BudgetCsvDownloader extends BaseDownloader {
    @Override
    public final List<Raw> downloadAndPopulateRawData(final Message message) {
        File folder = new File(message.getValue("path"));
//        File folder = new File("/Users/michalriha/Downloads/spain_budgets/stream/tabula-L_09_E_R1_edit");
        File[] listOfCsvs = folder.listFiles();

        final List<Raw> rawData = new ArrayList<>();

        for (File csv : listOfCsvs) {
            Raw file = rawDao.getEmptyInstance();

            file.setSourceFileName(csv.getName());
            try {
                file.setSourceData(new String(Files.readAllBytes(Paths.get(csv.getPath()))).replaceAll("\\u0000",
                        "").replace("\\u0000", ""));
            } catch (IOException e) {
                e.printStackTrace();
            }

            rawData.add(file);
        }

        return rawData;
    }

    @Override
    public final RawDAO<Raw> getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    @Override
    public final String getVersion() {
        return "1";
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
