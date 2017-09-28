package eu.dl.dataaccess.dao.hibernate;

import java.time.LocalDate;

import javax.persistence.NoResultException;
import javax.persistence.NonUniqueResultException;
import javax.persistence.Query;

import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.dataaccess.dto.raw.BasicCrawlerAuditRecord;

/**
 * Crawler Audit DAO implementation for MongoDB.
 */
public class HibernateCrawlerAuditDAO extends GenericHibernateDAO<BasicCrawlerAuditRecord>
        implements CrawlerAuditDAO<BasicCrawlerAuditRecord> {

    /**
     * Initializatizes the DAO.
     */
    public HibernateCrawlerAuditDAO() {
        super();
    }

    @Override
    public final LocalDate getLastCrawledDateByCrawler() {
        BasicCrawlerAuditRecord crawlerAuditRecord = getByNameAndVersion();

        if (crawlerAuditRecord != null) {
            return crawlerAuditRecord.getLastCrawledDate();
        } else {
            return null;
        }
    }

    @Override
    public final void updateLastCrawledDateForCrawler(final LocalDate lastCrawledDate) {
        BasicCrawlerAuditRecord crawlerAuditRecord = getByNameAndVersion();
        if (crawlerAuditRecord == null) {
            crawlerAuditRecord = new BasicCrawlerAuditRecord();
        }
        crawlerAuditRecord.setLastCrawledDate(lastCrawledDate);
        save(crawlerAuditRecord);
    }

    @Override
    public final Integer getLastCrawledPageNumberByCrawler() {
        BasicCrawlerAuditRecord crawlerAuditRecord = getByNameAndVersion();

        if (crawlerAuditRecord != null) {
            return crawlerAuditRecord.getLastCrawledPageNumber();
        } else {
            return null;
        }
    }

    @Override
    public final void updateLastCrawledPageNumberForCrawler(final Integer lastCrawledPageNumber) {
        BasicCrawlerAuditRecord crawlerAuditRecord = getByNameAndVersion();
        if (crawlerAuditRecord == null) {
            crawlerAuditRecord = new BasicCrawlerAuditRecord();
        }
        crawlerAuditRecord.setLastCrawledPageNumber(lastCrawledPageNumber);
        save(crawlerAuditRecord);
    }

    /**
     * Gets crawler audit record identified by name and version.
     * 
     * @return found entry or null
     */
    private BasicCrawlerAuditRecord getByNameAndVersion() {
        Query q = entityManager.createQuery(
                "SELECT car FROM BasicCrawlerAuditRecord car "
                        + "WHERE car.modifiedBy = :modifiedBy AND car.modifiedByVersion = :modifiedByVersion");
        q.setParameter("modifiedByVersion", getWorkerVersion());
        q.setParameter("modifiedBy", getWorkerName());

        try {
            BasicCrawlerAuditRecord crawlerAuditRecord = (BasicCrawlerAuditRecord) q.getSingleResult();
            return crawlerAuditRecord;
        } catch (NoResultException ex) {
            // nothing found
            return null;
        } catch (NonUniqueResultException ex) {
            logger.error("More entries for the same crawler name {} and version {} found.", getWorkerName(),
                getWorkerVersion());
            throw ex;
        }
    }

    @Override
    protected final Class<BasicCrawlerAuditRecord> getDTOClass() {
        return BasicCrawlerAuditRecord.class;
    }

    @Override
    public final BasicCrawlerAuditRecord getEmptyInstance() {
        return new BasicCrawlerAuditRecord();
    }
}
