package eu.dl.dataaccess.dao.mongo;

import java.time.LocalDate;

import org.mongojack.DBProjection;
import org.mongojack.DBQuery;
import org.mongojack.DBUpdate;

import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.dataaccess.dto.raw.BasicCrawlerAuditRecord;

/**
 * Crawler Audit DAO implementation for MongoDB.
 */
public class MongoCrawlerAuditDAO extends GenericMongoDAO<BasicCrawlerAuditRecord>
        implements CrawlerAuditDAO<BasicCrawlerAuditRecord> {
    private static final String CRAWLER_AUDIT_COLLECTION_NAME = "crawlerAudit";

    @Override
    protected final Class<BasicCrawlerAuditRecord> getDTOClass() {
        return BasicCrawlerAuditRecord.class;
    }

    @Override
    protected final String getCollectionName() {
        return CRAWLER_AUDIT_COLLECTION_NAME;
    }

    @Override
    public final LocalDate getLastCrawledDateByCrawler() {
        BasicCrawlerAuditRecord crawlerAuditRecord = getCollection().findOne(
                DBQuery.is("createdBy", getWorkerName()).is("createdByVersion", getWorkerVersion()),
                DBProjection.include("lastCrawledDate"));
        if (crawlerAuditRecord != null) {
            return crawlerAuditRecord.getLastCrawledDate();
        }
        return null;
    }

    @Override
    public final void updateLastCrawledDateForCrawler(final LocalDate lastCrawledDate) {
        DBUpdate.Builder update = new DBUpdate.Builder().set("lastCrawledDate", lastCrawledDate);
        populateUpdateWithMetadata(update);

        getCollection().findAndModify(DBQuery.is("createdBy", getWorkerName())
            .is("createdByVersion", getWorkerVersion()), null, null, false, update, true, true);
    }

    @Override
    public final Integer getLastCrawledPageNumberByCrawler() {
        BasicCrawlerAuditRecord crawlerAuditRecord = getCollection().findOne(
                DBQuery.is("createdBy", getWorkerName()).is("createdByVersion", getWorkerVersion()),
                DBProjection.include("lastCrawledPageNumber"));
        if (crawlerAuditRecord != null) {
            return crawlerAuditRecord.getLastCrawledPageNumber();
        }
        return null;
    }

    @Override
    public final void updateLastCrawledPageNumberForCrawler(final Integer lastCrawledPageNumber) {
        DBUpdate.Builder update = new DBUpdate.Builder().set("lastCrawledPageNumber", lastCrawledPageNumber);
        populateUpdateWithMetadata(update);

        getCollection().findAndModify(DBQuery.is("createdBy", getWorkerName())
            .is("createdByVersion", getWorkerVersion()), null, null, false, update, true, true);
    }
}
