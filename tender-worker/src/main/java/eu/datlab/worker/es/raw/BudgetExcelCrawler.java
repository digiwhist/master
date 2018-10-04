package eu.datlab.worker.es.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.raw.BaseHttpCrawler;

/**
 * Spanish excel documents crawler.
 */
public class BudgetExcelCrawler extends BaseHttpCrawler {
    private static final String VERSION = "1";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void doWork(final Message message) {
        for (Integer month = 1; month <= 12; month++) {
            for (Integer year = 2009; year <= 2015; year++) {
                final String url = String.format(
                        "http://serviciostelematicosext.minhap.gob.es/SGCAL/entidadeslocales/publicaciones/eell/" +
                                "EL%sC%s.xls", year, String.format("%02d", month));

                final Message outgoingMessage = MessageFactory.getMessage();
                outgoingMessage.setValue("binaryDataUrl", url);
                publishMessage(outgoingMessage);
                logger.info("New message sent to be processed: {}", outgoingMessage);
            }
        }
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
