package eu.datlab.worker.in.raw;

/**
 * Tender crawler for India searches tab 'Search' and tender's category 'Corrigendum'.
 *
 * @author Tomas Mrazek
 */
public final class CPPPSearchCorrigendumTenderCrawler extends BaseCPPPSearchTenderCrawler {
    @Override
    protected String getCategory() {
        return "corrigendum";
    }
}
