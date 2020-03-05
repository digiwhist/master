package eu.datlab.worker.in.raw;

import com.gargoylesoftware.htmlunit.html.HtmlListItem;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlSelect;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.BasicCrawlerAuditRecord;
import eu.dl.dataaccess.dto.raw.CrawlerAuditRecord;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseCrawler;
import eu.dl.worker.raw.utils.CrawlerUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.regex.Pattern;

/**
 * Base tender crawler for India searches Search tab.
 *
 * @author Tomas Mrazek
 */
public abstract class BaseCPPPSearchTenderCrawler extends BaseCrawler {
    private static final String VERSION = "1.0";

    private static final String SOURCE_DOMAIN = PublicationSources.IN_CPPP;

    private static final String SEARCH_FORM_URL = SOURCE_DOMAIN + "cppp/tendersearch";

    private static final int AJAX_WAITING_LIMIT = 30000;

    private final CrawlerAuditDAO auditDAO = DAOFactory.getDAOFactory().getCrawlerAuditDAO(getName(), getVersion());

    private static final int PAGE_SIZE = 10;

    /**
     * @return tender's category name
     */
    protected abstract String getCategory();

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void doWork(final Message message) {
        String regex = message.getValue("regex");
        if (regex != null) {
            // set crawler audit worker name for regex
            auditDAO.setWorkerName(getName() + "_" + regex.toLowerCase());
        }

        try {
            getTransactionUtils().begin();

            crawlTenderCategory(regex);

            getTransactionUtils().commit();
        } catch (final Exception e) {
            logger.error("Crawling failed", e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
    }

    /**
     * Method crawls all detail links of the given tender category.
     *
     * @param regex
     *      regex of begin organization name
     */
    private void crawlTenderCategory(final String regex) {
        logger.info("Crawling of tender category '{}' starts.", getCategory());

        // audit record initialization
        CrawlerAuditRecord auditRecord = initCrawlerAuditRecord();

        @SuppressWarnings("unchecked")
        Map<String, Map<String, Integer>> organizations =
            (Map<String, Map<String, Integer>>) auditRecord.getMetaData().get("organizations");
        String lastOrganization = (String) auditRecord.getMetaData().get("lastOrganizationName");

        // search form is protected by CAPTCHA
        HtmlPage actualPage = CPPPCrawlerUtils.getCAPTCHAProtectedPage(SEARCH_FORM_URL);

        // select category
        HtmlSelect categorySelect  = actualPage.getFirstByXPath("//select[@name='s_type']");
        if (categorySelect == null) {
            logger.error("Search form doesn't include 'Search Category' select");
            throw new UnrecoverableException("Search form doesn't include 'Search Category' select");
        }
        actualPage = categorySelect.setSelectedAttribute(getCategory(), true);

        // organization name select is loaded by AJAX after category selection
        CrawlerUtils.waitForElement(actualPage, isNotOrganizationSelectLoaded(), AJAX_WAITING_LIMIT);

        HtmlSelect organizationNameSelect  = actualPage.getFirstByXPath("//select[@name='s_state']");
        if (organizationNameSelect == null) {
            logger.error("Search form doesn't include 'Organization Name' select");
            throw new UnrecoverableException("Search form doesn't include 'Organization Name' select");
        }

        // pre-select last crawled organization if is set
        if (lastOrganization != null) {
            logger.info("Last organization name {} found in crawler audit record.", lastOrganization);
            organizationNameSelect.setSelectedAttribute(lastOrganization, true);

            if (organizationNameSelect.getSelectedIndex() == -1) {
                logger.error("Unable to find last crawled organization {} in select", lastOrganization);
                throw new UnrecoverableException("Unable to find last crawled organization");
            }
        } else {
            if (regex == null) {
                // index 0 includes '-- Select --'
                organizationNameSelect.setSelectedIndex(1);
            } else {
                int index = -1;
                //get first organization which name starts with character in a given regex
                Pattern p = Pattern.compile("(?i)^" + regex);
                for (int i = 1; i < organizationNameSelect.getOptionSize(); i++) {
                    if (p.matcher(organizationNameSelect.getOption(i).asText()).find()) {
                        index = i;
                        break;
                    }
                }

                if (index < 0) {
                    logger.error("No organization in select for regex {}", regex);
                    throw new UnrecoverableException("Unable to find organization for regex");
                } else {
                    organizationNameSelect.setSelectedIndex(index);
                }
            }
        }

        // select organization one by one and crawl all its detail links. first option (index 0) includes --Select--.
        for (int i = organizationNameSelect.getSelectedIndex(); i < organizationNameSelect.getOptionSize(); i++) {
            // select organization
            organizationNameSelect.setSelectedIndex(i);

            String organizationName = organizationNameSelect.getOption(i).getText();

            // check whether the organization name is in regex if needed
            if (regex != null && !Pattern.compile("(?i)^" + regex).matcher(organizationName).find()) {
                break;
            }

            logger.info("Crawling of organization '{}' starts.", organizationName);

            // submit search form
            HtmlPage resultsPage = CrawlerUtils.clickElement(actualPage, "//input[@id='edit-save']");

            int page = 1;
            int skip = 0;
            boolean skipped = false;
            // get starting page for crawled organization
            if (organizations.containsKey(organizationName)) {
                page = organizations.get(organizationName).get("page");
                // modulo ensures no skipping in case that all records was crawled
                skip = organizations.get(organizationName).get("row") % PAGE_SIZE;
                // start on the next page in case that all records was crawled
                if (organizations.get(organizationName).get("row") == PAGE_SIZE) {
                    page++;
                }
            }

            if (page > 1) {
                logger.info("Last page number {} found in crawler audit record.", page);
                // To URL of first page add page parameter with required page number. Unable to use page button, button of required page may
                // not be on the first page.
                resultsPage =  CPPPCrawlerUtils.getNthPage(resultsPage, page);
            }

            if (skip > 0) {
                logger.info("Last row number {} from page {} found in crawler audit record.", skip, page);
            }

            do {
                if (!verifyPageNumber(resultsPage, page)) {
                    break;
                }

                logger.info("Processing page #{} with url {} and params {}", page, resultsPage.getUrl(),
                    resultsPage.getWebResponse().getWebRequest().getRequestParameters());

                List<Message> messages = CPPPCrawlerUtils.extractDetailsFromPage(resultsPage, 1, 4);
                if (messages.isEmpty()) {
                    // create crawler audit record if needed
                    if (lastOrganization == null) {
                        updateCrawlerAuditRecord(auditRecord, organizationName, page, 0);
                    }

                    break;
                }

                messages.stream()
                    .skip(skipped ? 0 : skip)
                    .forEach(m -> {
                        // metadata shouldn't be NULL because of CPPPCrawlerUtils.extractDetailsFromPage returns messages with set metadata.
                        HashMap<String, Object> metaData = m.getMetaData();
                        metaData.put("organizationName", organizationName);
                        metaData.put("category", getCategory());

                        publishMessage(m);
                    });

                // update last crawled record with page and organization name
                updateCrawlerAuditRecord(auditRecord, organizationName, page, messages.size());

                skipped = true;
                page++;
                resultsPage = CPPPCrawlerUtils.getNthPage(resultsPage, page);

                logger.debug("Data extracted, moving to next page.");
            } while(resultsPage != null);
        }

        logger.info("Finished.");
    }

    /**
     * Verifies the page number. Note that if the crawler attempts to get page greater than last page, server returns last page.
     *
     * @param page
     *      current page
     * @param pageNumber
     *      verified page number
     * @return TRUE if and only if the verified page number is smaller or equal to page number parsed from pagination bar of given page,
     *      otherwise FALSE
     * @throws UnrecoverableException in case that current page button is not found on the given page.
     */
    private boolean verifyPageNumber(final HtmlPage page, final int pageNumber) {
        HtmlListItem currentPageButton = page.getFirstByXPath("//li[@class='pager__item is-active']");
        if (currentPageButton != null) {
            if (pageNumber > Integer.valueOf(currentPageButton.asText().replaceAll("\\D+", ""))) {
                return false;
            }
        } else {
            // if organization's records take only one page, the pagination bar is not displayed
            if (pageNumber > 1) {
                return false;
            }
        }

        return true;
    }

    /**
     * Updates last crawler audit record with page number a nd organization name.
     *
     * @param record
     *      crawler audit record to be updated
     * @param name
     *      last crawled organization name
     * @param page
     *      last crawled page
     * @param row
     *      number of latest crawled row
     */
    private void updateCrawlerAuditRecord(final CrawlerAuditRecord record, final String name, final int page, final int row) {
        HashMap<String, Object> metaData = record.getMetaData();

        @SuppressWarnings("unchecked")
        HashMap<String, Map<String, Integer>> organizations = (HashMap<String, Map<String, Integer>>) metaData.get("organizations");

        metaData.put("lastOrganizationName", name);

        Map<String, Integer> pageAndRow = new HashMap<>();
        pageAndRow.put("page", page);
        pageAndRow.put("row", row);
        organizations.put(name, pageAndRow);

        record.setMetaData(metaData);
        String saveId = auditDAO.save(record);

        // set id for new audit record
        record.setId(saveId);
    }

    /**
     * @return TRUE if the page doesn't include organization select
     */
    private Predicate<HtmlPage> isNotOrganizationSelectLoaded() {
        return p -> p.getFirstByXPath("//select[@name='s_state']") == null;
    }

    /**
     * @return audit record with prerequisites
     */
    private CrawlerAuditRecord initCrawlerAuditRecord() {
        CrawlerAuditRecord auditRecord = (CrawlerAuditRecord) auditDAO.getByNameAndVersion();
        if (auditRecord == null) {
            auditRecord = new BasicCrawlerAuditRecord();
        }
        if (auditRecord.getMetaData() == null) {
            auditRecord.setMetaData(new HashMap<>());
        }
        if (!auditRecord.getMetaData().containsKey("organizations")) {
            auditRecord.getMetaData().put("organizations", new HashMap<String, Map<String, Integer>>());
        }

        return auditRecord;
    }
}
