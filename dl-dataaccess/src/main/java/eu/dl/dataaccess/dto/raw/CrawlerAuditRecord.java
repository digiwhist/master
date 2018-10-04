package eu.dl.dataaccess.dto.raw;

import java.time.LocalDate;

import eu.dl.dataaccess.dto.Storable;

/**
 * Crawler audit record with metadata about crawler runs. Used to continuosly
 * crawle data from different sources.
 */
public interface CrawlerAuditRecord extends Storable {

    /**
     * Gets the last date that the crawler proceeded.
     *
     * @return the last date that the crawler proceeded
     */
    LocalDate getLastCrawledDate();

    /**
     * Sets the last date that the crawler proceeded.
     *
     * @param lastCrawledDate
     *         the last date that the crawler proceeded
     *
     * @return this instance for chaining
     */
    CrawlerAuditRecord setLastCrawledDate(LocalDate lastCrawledDate);

    /**
     * Gets the last page number that the crawler proceeded.
     *
     * @return the last page number that the crawler proceeded
     */
    Integer getLastCrawledPageNumber();

    /**
     * Sets the last page number that the crawler proceeded.
     *
     * @param lastCrawledPageNumber
     *         the last page number that the crawler proceeded
     *
     * @return this instance for chaining
     */
    CrawlerAuditRecord setLastCrawledPageNumber(Integer lastCrawledPageNumber);
}