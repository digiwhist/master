package eu.datlab.worker.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.raw.BaseIncrementalPagedSourceHttpCrawler;

/**
 * Base class for incremental crawlers for paged HTTP sources.
 */
public abstract class BaseDatlabIncrementalPagedSourceHttpCrawler extends BaseIncrementalPagedSourceHttpCrawler {
    @Override
    protected final CrawlerAuditDAO getCrawlerAuditDAO() {
        return DAOFactory.getDAOFactory().getCrawlerAuditDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
