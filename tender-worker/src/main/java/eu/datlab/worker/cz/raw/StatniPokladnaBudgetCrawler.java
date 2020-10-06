package eu.datlab.worker.cz.raw;

import java.time.LocalDate;
import java.time.Month;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.Map;

import eu.datlab.worker.raw.BaseDatlabIncrementalCrawler;

/**
 * This class is searching http://monitor.statnipokladna.cz/2016/zdrojova-data/transakcni-data for the fourth quarter
 * budget datasets from selected categories.
 * 
 * @author Tomas Mrazek
 */
public final class StatniPokladnaBudgetCrawler extends BaseDatlabIncrementalCrawler {

    private static final String VERSION = "1.0";

    private static final String SOURCE_URL = "https://monitor.statnipokladna.cz";
    private static final String DATA_URL = SOURCE_URL + "/data/extrakty/csv";
    
    private static final LocalDate DEFAUL_START_DATE = LocalDate.of(2010, Month.JANUARY, 1);

    private static final String DATASET_NAME_PATTERN = "/%s/%d_12_Data_CSUIS_%s.zip";
    
    private static final Map<String, String> DATASET_INDENTIFIERS = new HashMap<>();
    static {
        //cities budgets (FIN 2-12M - Plnění rozpočtu MŘO)
        DATASET_INDENTIFIERS.put("FINM", "FinM");
        //state organisations budgets (FIN 2-04U - Plnění rozpočtu KAP a OSS)
        DATASET_INDENTIFIERS.put("MISRIS", "FinOSS");
        //profit and loss (Výkaz zisku a ztráty)
        DATASET_INDENTIFIERS.put("VYKZZ", "ZiskZtraty");
        //balance sheets (Rozvaha)
        DATASET_INDENTIFIERS.put("ROZV", "Rozvaha");
        //old state organisations budgets (FIN 2-04U - Plnění rozpočtu KAP, OSS a SF (2010 - 2014))
        DATASET_INDENTIFIERS.put("FINU", "FinU");
        // cash flow (Přehled peněžních toků)
        DATASET_INDENTIFIERS.put("PPT", "PenezniToky");
    }
    
    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        crawlDatasetsForYear(date.getYear());
    }

    /**
     * Generates URLs for budget items datasets from each category and for the given {@code year}.
     *
     * @param year 
     *      datatset year
     */
    private void crawlDatasetsForYear(final int year) {
        for (Map.Entry<String, String> n : DATASET_INDENTIFIERS.entrySet()) {
            final String dataset = String.format(DATASET_NAME_PATTERN, n.getValue(), year, n.getKey());
            createAndPublishMessage(DATA_URL + dataset);
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
