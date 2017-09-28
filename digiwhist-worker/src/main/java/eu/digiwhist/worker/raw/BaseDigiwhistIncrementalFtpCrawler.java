package eu.digiwhist.worker.raw;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.raw.BaseIncrementalFtpCrawler;

/**
 * Base Digiwhist class for incremental crawlers for paged FTP sources.
 */
public abstract class BaseDigiwhistIncrementalFtpCrawler extends BaseIncrementalFtpCrawler {
    @Override
    protected final CrawlerAuditDAO getCrawlerAuditDAO() {
        return DAOFactory.getDAOFactory().getCrawlerAuditDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
