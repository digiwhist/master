package eu.datlab.worker.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.raw.BaseIncrementalFtpCrawler;

/**
 * Base class for incremental crawlers for paged FTP sources.
 */
public abstract class BaseDatlabIncrementalFtpCrawler extends BaseIncrementalFtpCrawler {
    @Override
    protected final CrawlerAuditDAO getCrawlerAuditDAO() {
        return DAOFactory.getDAOFactory().getCrawlerAuditDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
