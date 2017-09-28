package eu.digiwhist.worker.hr.raw;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.remote.CapabilityType;
import org.openqa.selenium.remote.DesiredCapabilities;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.FluentWait;
import org.openqa.selenium.support.ui.Wait;
import org.openqa.selenium.support.ui.WebDriverWait;

import com.google.common.base.Function;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;

/**
 * Downloads detail html from detail page. In detail page is located download button.
 *
 * @author Kuba Krafka
 */
public final class EOJNTenderDownloader extends BaseDownloader<RawData> {
    private static final String VERSION = "3";

    private static final String WHOLE_FORM_BUTTON_ID = "uiDokumentPodaci_uiDocumentCtl_uiOpenDocumentHtml";

    private final WebDriver driver;
    private final String downloadFilepath;

    /**
     * Default constructor. Sets web client to disable javascript.
     */
    public EOJNTenderDownloader() {
        super();

        System.setProperty("webdriver.chrome.driver", "/tmp/chromedriver");

        String uuid = UUID.randomUUID().toString();
        downloadFilepath = "/tmp/" + getName() + uuid;

        HashMap<String, Object> chromePrefs = new HashMap<String, Object>();
        chromePrefs.put("profile.default_content_settings.popups", 0);
        chromePrefs.put("download.default_directory", downloadFilepath);

        ChromeOptions options = new ChromeOptions();
        options.setExperimentalOption("prefs", chromePrefs);

        // TODO: enters into the headless mode, waits for bugfix
        // there is reported a bug,
        // remove once fixed here https://bugs.chromium.org/p/chromium/issues/detail?id=696481

        // options.addArguments("--headless");
        // options.addArguments("--disable-gpu");

        DesiredCapabilities cap = DesiredCapabilities.chrome();
        cap.setCapability(CapabilityType.ACCEPT_SSL_CERTS, true);
        cap.setCapability(ChromeOptions.CAPABILITY, options);

        driver = new ChromeDriver(cap);
    }

    @Override
    public List<RawData> downloadAndPopulateRawData(final Message message) {
        // init raw data
        final RawData rawData = rawDao.getEmptyInstance();

        // get message parameters
        final String sourceDataUrl = message.getValue("url");

        // download data and populate raw data object
        if (sourceDataUrl != null) {
            try {
                rawData.setSourceData(download(sourceDataUrl));
                rawData.setSourceUrl(new URL(sourceDataUrl));
            } catch (final Exception e) {
                logger.error("Downloading failed for page with url {} with exception {}", sourceDataUrl, e);
                throw new UnrecoverableException("Unable to download source data.", e);
            }
        } else {
            logger.error("No URL found in the message: {}", message);
            throw new UnrecoverableException("No URL provided.");
        }

        return Arrays.asList(rawData);
    }

    /**
     * Download file from URL with help of selenium.
     *
     * @param url file url
     *
     * @return downloaded file contents
     */
    private String download(final String url) {
        driver.get(url);

        // wait until button is visible
        WebDriverWait waitForDownloadButton = new WebDriverWait(driver, 30);
        WebElement downloadButton = waitForDownloadButton.until(
                ExpectedConditions.visibilityOfElementLocated(By.id(WHOLE_FORM_BUTTON_ID)));

        downloadButton.click();

        Wait<String> waitFowDownload = new FluentWait<String>(downloadFilepath)
                .withTimeout(120, TimeUnit.SECONDS)
                .pollingEvery(500, TimeUnit.MILLISECONDS);

        String fileContent = waitFowDownload.until(new Function<String, String>() {
            public String apply(final String downloadPath) {
                assert downloadPath.equals(downloadFilepath);
                File folder = new File(downloadPath);

                if (!folder.exists()) {
                    // folder does not exist yet -> wait
                    return null;
                }

                for (File file : folder.listFiles()) {
                    if (file.getName().contains(".Html") && !file.getName().contains(".crdownload")) {
                        try {
                            String fileContent = new String(Files.readAllBytes(Paths.get(file.getPath())));
                            file.delete();
                            folder.delete();
                            return fileContent;
                        } catch (IOException e) {
                            logger.error("Unable to read file contents.", e);
                            throw new UnrecoverableException("Unable to read file contents", e);
                        }
                    }
                }

                // folder does not contain the HTML file yet -> wait
                return null;
            }
        });

        return fileContent;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    public RawDAO getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

}
