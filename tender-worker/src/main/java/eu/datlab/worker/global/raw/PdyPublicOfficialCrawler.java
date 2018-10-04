package eu.datlab.worker.global.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.worker.global.PublicOfficialUtils.EuCountry;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseHttpCrawler;

/**
 * Crawler for public officials Political Data Yearbook.
 *
 * @author Michal Riha
 */
public final class PdyPublicOfficialCrawler extends BaseHttpCrawler {
    private static final String VERSION = "1";

    @Override
    protected void doWork(final Message message) {
        for (EuCountry country : EuCountry.values()) {
            final String countryDataUrl = String.format(
                    "http://www.politicaldatayearbook.com/ChartDataCsv"
                            + ".aspx?chartGroup=MINISTRY&countryId=%d&chartSystemName=MINISTRY_EACH",
                    country.getCountryNumber());
            createAndPublishMessage(countryDataUrl);
        }
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
