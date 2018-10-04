package eu.datlab.worker.eu.raw;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.raw.BaseHttpCrawler;

/**
 * Crawler for asset declarations for Members of European Parliament.
 */
public final class MEPAssetDeclarationCrawler extends BaseHttpCrawler {
    private static final String VERSION = "1";
    private static final int TIMEOUT = 10000;
    private static final String MEP_COMPLETE_LIST_URL = "http://www.europarl.europa.eu/meps/en/full-list" + ""
            + ".html?filter=all&leg=";

    @Override
    protected void doWork(final Message message) {
        // TODO: should use HTMLUnit instead of Jsoup
        try {
            final Document doc = Jsoup.connect(MEP_COMPLETE_LIST_URL).timeout(TIMEOUT).get();

            List<String> detailsLinks = getLinksToMEPDetails(doc);
            for (String detailLink : detailsLinks) {
                List<String> filesUrls = getAssetDeclarationFilesUrls(detailLink);
                for (String fileUrl : filesUrls) {
                    final Message outgoingMessage = MessageFactory.getMessage();
                    outgoingMessage.setValue("binaryDataUrl", fileUrl);
                    publishMessage(outgoingMessage);
                    logger.info("New message {} with url {} sent to be processed", outgoingMessage, fileUrl);
                }
            }
        } catch (IOException e) {
            logger.error("Connecting to MEP list {} failed.", MEP_COMPLETE_LIST_URL, e);
            throw new UnrecoverableException("Crawling failed while connecting to MEP list page.", e);
        }
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    /**
     * Parses links to detail pages for all the MEPs listed on the provided site.
     *
     * @param doc
     *         parsed page with the list of MEPs
     *
     * @return links to MEPs' detail pages
     */
    private static List<String> getLinksToMEPDetails(final Document doc) {
        List<String> detailsLinks = new ArrayList<>();
        Elements mepInfoDivs = doc.select("div.zone_info_mep");
        for (Element mepInfo : mepInfoDivs) {
            Element detailAnchor = mepInfo.select("a").first();
            if (detailAnchor != null) {
                detailsLinks.add(detailAnchor.attr("abs:href"));
            }
        }
        return detailsLinks;
    }

    /**
     * Uses given MEP detail URL to construct URL for page with details about declarations and parses URLs for all
     * published asset declarations.
     *
     * @param mepDetailLink
     *         URL to page with details about member of the European Parliament
     *
     * @return URLs of PDF files with asset declarations
     */
    private List<String> getAssetDeclarationFilesUrls(final String mepDetailLink) {
        List<String> filesUrls = new ArrayList<>();
        String mepDeclarationsUrl = mepDetailLink.replaceFirst("home", "declarations");
        try {
            Document declarationsPage = Jsoup.connect(mepDeclarationsUrl).timeout(TIMEOUT).get();
            Elements assetDeclarationFilesAnchors = declarationsPage.select("h3#sectionDIF + div a.link_pdf");
            filesUrls.addAll(assetDeclarationFilesAnchors.stream()
                    .map(assetDeclarationFileAnchor -> assetDeclarationFileAnchor.attr("abs:href"))
                    .collect(Collectors.toList()));
        } catch (IOException e) {
            logger.error("Connecting to declarations page {} failed.", mepDeclarationsUrl, e);
            throw new UnrecoverableException("Crawling failed while connecting to declarations page.", e);
        }
        return filesUrls;
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
