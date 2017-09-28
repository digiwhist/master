package eu.dl.dataaccess.dto.raw;

import java.time.LocalDate;

import javax.persistence.Entity;
import javax.persistence.Table;

import eu.dl.dataaccess.dto.StorableDTO;

/**
 * Crawler audit record with metadata about crawler runs.
 */
@Entity
@Table(name = "crawler_audit_record")
public final class BasicCrawlerAuditRecord extends StorableDTO implements CrawlerAuditRecord {

    private LocalDate lastCrawledDate;
    private Integer lastCrawledPageNumber;

    /* (non-Javadoc)
     * @see eu.dl.dataaccess.dto.raw.basic.CrawlerAuditRecord#getLastCrawledDate()
     */
    @Override
    public LocalDate getLastCrawledDate() {
        return lastCrawledDate;
    }

    /* (non-Javadoc)
     * @see eu.dl.dataaccess.dto.raw.basic.CrawlerAuditRecord#setLastCrawledDate(java.time.LocalDate)
     */
    @Override
    public BasicCrawlerAuditRecord setLastCrawledDate(final LocalDate lastCrawledDate) {
        this.lastCrawledDate = lastCrawledDate;
        return this;
    }

    /* (non-Javadoc)
     * @see eu.dl.dataaccess.dto.raw.basic.CrawlerAuditRecord#getLastCrawledPageNumber()
     */
    @Override
    public Integer getLastCrawledPageNumber() {
        return lastCrawledPageNumber;
    }

    /* (non-Javadoc)
     * @see eu.dl.dataaccess.dto.raw.basic.CrawlerAuditRecord#setLastCrawledPageNumber(java.lang.Integer)
     */
    @Override
    public BasicCrawlerAuditRecord setLastCrawledPageNumber(final Integer lastCrawledPageNumber) {
        this.lastCrawledPageNumber = lastCrawledPageNumber;
        return this;
    }
}
