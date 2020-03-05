package eu.datlab.worker.in.raw;

/**
 * Tender crawler for India searches tab 'Search' and tender's category 'Archive'.
 *
 * @author Tomas Mrazek
 */
public final class CPPPSearchArchiveTenderCrawler extends BaseCPPPSearchTenderCrawler {
    @Override
    protected String getCategory() {
        return "archivedtenders";
    }
}
