package eu.dl.dataaccess.dao.jdbc;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.dataaccess.dto.raw.BasicCrawlerAuditRecord;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.time.LocalDate;

/**
 * Crawler Audit DAO implementation for JDBC.
 */
public class JdbcCrawlerAuditDAO extends GenericJdbcDAO<BasicCrawlerAuditRecord> implements CrawlerAuditDAO<BasicCrawlerAuditRecord> {

    private static final String TABLE_NAME = "crawler_audit_record";

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

    @Override
    public final BasicCrawlerAuditRecord getByNameAndVersion() {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE createdby = ? AND createdByVersion = ?");

            statement.setString(1, getWorkerName());
            statement.setString(2, getWorkerVersion());
            statement.executeQuery();

            ResultSet rs = statement.executeQuery();

            BasicCrawlerAuditRecord result = getEmptyInstance();

            if (rs.next()) {
                result = createFromResultSet(rs);
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final BasicCrawlerAuditRecord getEmptyInstance() {
        return new BasicCrawlerAuditRecord();
    }

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }
}
