package eu.digiwhist.worker.cz.raw;

import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.List;

import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.digiwhist.worker.raw.BaseDigiwhistIncrementalCrawler;
import eu.dl.core.UnrecoverableException;

/**
 * This class is searching http://monitor.statnipokladna.cz/2016/zdrojova-data/transakcni-data for the fourth quarter
 * budget datasets from selected categories.
 * 
 * @author Tomas Mrazek
 */
public final class StatniPokladnaBudgetCrawler extends BaseDigiwhistIncrementalCrawler {

    private static final String VERSION = "1.0";
    
    private static final String SOURCE_URL = "http://monitor.statnipokladna.cz";
    
    private static final String DATA_URL = SOURCE_URL + "/2016/zdrojova-data/transakcni-data";
    
    private static final LocalDate DEFAUL_START_DATE = LocalDate.of(2010, Month.JANUARY, 1);

    private static final String DATASET_URL_PATTERN = "/data/%d_12_Data_CSUIS_%s.zip";
    
    private static final List<String> DATASET_INDENTIFIERS = Arrays.asList(
        //cities budgets (FIN 2-12M - Plnění rozpočtu MŘO)
        "FINM",
        //state organisations budgets (FIN 2-04U - Plnění rozpočtu KAP a OSS)
        "MISRIS",
        //profit and loss (Výkaz zisku a ztráty)
        "VYKZZ",
        //balance sheets (Rozvaha)
        "ROZV",
        //old state organisations budgets (FIN 2-04U - Plnění rozpočtu KAP, OSS a SF (2010 - 2014))
        "FINU");
    
    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        final WebClient webClient = new WebClient();
        webClient.getOptions().setUseInsecureSSL(true);
        webClient.getOptions().setThrowExceptionOnScriptError(false);
        webClient.getOptions().setJavaScriptEnabled(false);
        
        try {
            final HtmlPage page = webClient.getPage(DATA_URL);
            
            crawlDatasetsForYear(page, date.getYear());            
        } catch(IOException e) {
            logger.error("Crawling failed for date {} because of", date, e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
    }

    /**
     * Generates URLs for budget items datasets from each category and for the given {@code year}.
     *
     * @param page
     *      HTML page that includes datatsets URLs
     * @param year 
     *      datatset year
     */
    private void crawlDatasetsForYear(final HtmlPage page, final int year) {
        for (String datasetId : DATASET_INDENTIFIERS) {
            final String datasetName = String.format(DATASET_URL_PATTERN, year, datasetId);
            final HtmlAnchor datasetAnchor = (HtmlAnchor) page.getFirstByXPath("//div[@id='content-panel']"
                + "//a[@href='" + datasetName + "']");

            if (datasetAnchor != null) {
                createAndPublishMessage(SOURCE_URL + datasetAnchor.getHrefAttribute());
            }
        }
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return DEFAUL_START_DATE;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.YEARS;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected void initialSetup() {
    }

    @Override
    protected void finalCleanup() {
    }
}
