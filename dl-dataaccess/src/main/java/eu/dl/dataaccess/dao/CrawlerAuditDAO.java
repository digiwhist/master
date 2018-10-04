package eu.dl.dataaccess.dao;

import java.time.LocalDate;

/**
 * Crawler Audit DAO interface. Specifies methods for storing and loading audit
 * data about crawlers.
 * 
 * @param <T>
 *            item with crawling records
 */
public interface CrawlerAuditDAO<T> {
    /**
     * Saves the audit record.
     *
     * @param crawlerAuditRecord
     *         crawler audit record to be saved
     *
     * @return Id of saved audit record
     */
    String save(T crawlerAuditRecord);

    /**
     * Returns the last crawled date.
     *
     * @return the last crawled date or null if none exists
     */
    LocalDate getLastCrawledDateByCrawler();

    /**
     * Updates the last crawled date or creates new audit record if none exists.
     *
     * @param lastCrawledDate
     *         last crawled date to be set
     */
    void updateLastCrawledDateForCrawler(LocalDate lastCrawledDate);

    /**
     * Returns the last crawled page number.
     *
     * @return the last crawled page number or null if none exists
     */
    Integer getLastCrawledPageNumberByCrawler();

    /**
     * Updates the last crawled page number or creates new audit record if none exists.
     *
     * @param lastCrawledPageNumber
     *         last crawled page number to be set
     */
    void updateLastCrawledPageNumberForCrawler(Integer lastCrawledPageNumber);

    /**
     * Set source name to be used for metadata info.
     *
     * @param sourceName
     *            name of the source(worker for example)
     */
    void setWorkerName(String sourceName);

    /**
     * Gets crawler audit record identified by name and version.
     *
     * @return found entry or null
     */
    T getByNameAndVersion();
}
