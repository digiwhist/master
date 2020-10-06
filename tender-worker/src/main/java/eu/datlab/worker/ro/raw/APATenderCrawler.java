package eu.datlab.worker.ro.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTableDataCell;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.dataaccess.dao.DummyTransactionUtils;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.CrawlerAuditRecord;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.raw.BaseHttpCrawler;

import java.io.IOException;
import java.net.MalformedURLException;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

/**
 * Crawler for Romanian tenders.
 */
public class APATenderCrawler extends BaseHttpCrawler {
    private static final String VERSION = "1";
    private static final String[] PAGE_URLS = new String[]{
            "http://data.gov.ro/dataset/achizitii-publice-2007-2016-contracte6",
            "http://data.gov.ro/dataset/achizitii-publice-2010-2015-anunturi-de-participare",
            "http://data.gov.ro/dataset/achizitii-publice-2016",
            "http://data.gov.ro/dataset/achizitii-publice-2017",
            "http://data.gov.ro/dataset/achizitii-publice-2018",
            "http://data.gov.ro/dataset/achiziti-publice-2019",
            "https://data.gov.ro/dataset/achizitii-publice-2020"
    };

    private final CrawlerAuditDAO<CrawlerAuditRecord> auditDAO = DAOFactory.getDAOFactory().getCrawlerAuditDAO(getName(), getVersion());

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void doWork(final Message message) {
        LocalDate lastCrawledDate = auditDAO.getLastCrawledDateByCrawler();
        LocalDate newLastCrawledDate = null;
        for (String pageUrl : PAGE_URLS) {
            String[] years = pageUrl.replaceAll("[^\\-0-9]", "").split("-");
            Integer maxYear =
                    Arrays.stream(years).filter(a -> a.length() == 4 && a.startsWith("20"))
                            .map(Integer::parseInt).max(Integer::compareTo).orElse(null);
            if (lastCrawledDate != null && maxYear != null && (lastCrawledDate.getYear()) > maxYear) {
                continue;
            }
            try {
                final HtmlPage resultPage = getWebClient().getPage(pageUrl);
                final List<HtmlAnchor> csvAndXlsLinks = resultPage.getByXPath("//a[@class='heading']");

                if (csvAndXlsLinks.isEmpty()) {
                    logger.warn("Crawling of page {} failed, no files to crawl", pageUrl);
                }

                for (HtmlAnchor link : csvAndXlsLinks) {
                    String detailPageUrl = link.getHrefAttribute();
                    if (detailPageUrl != null && !detailPageUrl.isEmpty()) {
                        detailPageUrl = "https://data.gov.ro" + detailPageUrl;
                        final HtmlPage detailPage = getWebClient().getPage(detailPageUrl);
                        final List<HtmlTableDataCell> createdAnchor =
                                detailPage.getByXPath("//tr[th='Created']/td");
                        if (createdAnchor == null || createdAnchor.isEmpty()) {
                            continue;
                        }
                        LocalDate date = cleanDate(createdAnchor.get(0).getFirstChild().getTextContent());
                        if (date == null) {
                            final List<HtmlTableDataCell> modifiedAnchor =
                                    detailPage.getByXPath("//tr[th='Ultima actualizare']/td");
                            if (modifiedAnchor == null || modifiedAnchor.isEmpty()) {
                                continue;
                            }
                            date = cleanDate(modifiedAnchor.get(0).getFirstChild().getTextContent());
                            if (date == null) {
                                continue;
                            }
                        }
                        if (lastCrawledDate == null || date.compareTo(lastCrawledDate) > 0) {
                            newLastCrawledDate = date;
                            final List<HtmlAnchor> urlAnchor =
                                    detailPage.getByXPath("//a[@class='resource-url-analytics']");
                            if (urlAnchor != null && !urlAnchor.isEmpty()) {
                                String innerUrl = urlAnchor.get(0).getHrefAttribute();
                                final Message outgoingMessage = MessageFactory.getMessage();
                                if (innerUrl.contains(".csv")) {
                                    createAndPublishMessage(innerUrl);
                                } else {
                                    outgoingMessage.setValue("binaryDataUrl", innerUrl);
                                    publishMessage(outgoingMessage);
                                    logger.info("New message sent to be processed: {}", outgoingMessage);
                                }
                            }
                        }
                    }
                }
            } catch (MalformedURLException e) {
                e.printStackTrace();

            } catch (IOException e) {
                logger.error("Crawling failed with exception {}", e);
                throw new UnrecoverableException("Crawling failed", e);
            }
            if (newLastCrawledDate != null && (lastCrawledDate == null || lastCrawledDate.compareTo(newLastCrawledDate) < 0)) {
                auditDAO.updateLastCrawledDateForCrawler(newLastCrawledDate);
                lastCrawledDate = newLastCrawledDate;
            }
        }
    }


    /**
     * Cleans RO date to LocalDate.
     *
     * @param date date
     * @return clean date.
     */
    private static LocalDate cleanDate(final String date) {
        if (date == null || date.isEmpty()) {
            return null;
        }
        String[] parts = date.split(" ");
        if (parts.length != 3) {
            return null;
        }
        int month = 0;
        switch (parts[1].toLowerCase()) {
            case "ianuarie":
                month = 1;
                break;
            case "februarie":
                month = 2;
                break;
            case "martie":
                month = 3;
                break;
            case "aprilie":
                month = 4;
                break;
            case "mai":
                month = 5;
                break;
            case "iunie":
                month = 6;
                break;
            case "iulie":
                month = 7;
                break;
            case "august":
                month = 8;
                break;
            case "septembrie":
                month = 9;
                break;
            case "octombrie":
                month = 10;
                break;
            case "noiembrie":
                month = 11;
                break;
            case "decembrie":
                month = 12;
                break;
            default:
                break;
        }
        if (month == 0) {
            return null;
        }
        return LocalDate.of(Integer.parseInt(parts[2]), month, Integer.parseInt(parts[0]));
    }


    @Override
    protected final TransactionUtils getTransactionUtils() {
        return new DummyTransactionUtils();
    }
}
