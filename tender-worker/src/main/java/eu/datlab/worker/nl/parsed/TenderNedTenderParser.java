package eu.datlab.worker.nl.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * Tender parser for TenderNed in Netherlands.
 *
 * @author Marek Mikes
 */
public class TenderNedTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1";

    private static final String SOURCE_DOMAIN = PublicationSources.NL_NED;

    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        // set common attributes independent on pages content
        ParsedTender parsedTender = new ParsedTender()
                .addPublication(new ParsedPublication()
                        .setIsIncluded(true)
                        .setHumanReadableUrl(rawTender.getSourceUrl().toString())
                        .setSource(SOURCE_DOMAIN));

        // get overview page
        @SuppressWarnings("unchecked") final LinkedHashMap<String, String> metaData =
                (LinkedHashMap<String, String>) rawTender.getMetaData().get("additionalUrls");
        assert !metaData.isEmpty();
        Document overviewPage = null;
        for (String key : metaData.keySet()) {
            if (key.contains("samenvatting")) {
                overviewPage = Jsoup.parse(metaData.get(key));
                break;
            }
        }
        assert overviewPage != null;

        // TenderNed was temporarily unavailable
        if (overviewPage.title().equals("Storing TenderNed")) {
            logger.error("TenderNed was temporarily unavailable, so the overview page contains only their apology");
            return Arrays.asList(parsedTender);
        }

        // set common attributes dependent on overview page content
        parsedTender.getPublications().get(0)
                .setPublicationDate(parsePublicationDate(overviewPage))
                .setSourceFormType(parsePublicationSourceFormType(overviewPage))
                .setSourceTenderId(parsePublicationSourceTenderId(overviewPage));

        final Document page = Jsoup.parse(rawTender.getSourceData());

        // TenderNed was temporarily unavailable
        if (page.title().equals("Storing TenderNed")) {
            logger.error("TenderNed was temporarily unavailable, so the form page contains only their apology");
            return Arrays.asList(parsedTender);
        }
        // some forms could not be loaded -> probably it's some kind of bug on TenderNed
        // E.g. https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
        // d4f93b4272ab08c5e6c8d334efcf54d1/pageId/D909A/huidigemenu/aankondigingen/cid/931296/cvp/join
        if (!page.select("div.notification.error").isEmpty()) {
            logger.error("Page is empty and error is displayed");
            return Arrays.asList(parsedTender);
        }
        // some forms are written in English -> probably it's some kind of bug on TenderNed
        // E.g. https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
        // dad44353bff5ed9a1e2a033830db791e/pageId/D909A/huidigemenu/aankondigingen/cid/931459/cvp/join
        if (JsoupUtils.selectText("a[href='#top-publication']", page).equals("Go to top")) {
            logger.error("Form is written in English, but we are ready only for Dutch language");
            return Arrays.asList(parsedTender);
        }

        // parse form specific attributes
        assert page.select("div[id='top-publication']").size() == 1;
        final Element form = page.select("div[id='top-publication']").get(0);
        PublicationFormType formType = getFormType(parsedTender.getPublications().get(0).getSourceFormType());
        switch (formType) {
            case CONTRACT_NOTICE:
                return TenderNedTenderContractNoticeHandler.parse(parsedTender, form);
            case CONTRACT_AWARD:
                return TenderNedTenderContractAwardHandler.parse(parsedTender, form);
            default:
                // no operation
                return Arrays.asList(parsedTender);
        }
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    /**
     * Parse publication date from overview page.
     *
     * @param overviewPage
     *         overview page where publication date is
     *
     * @return String or Null
     */
    private static String parsePublicationDate(final Document overviewPage) {
        return JsoupUtils.selectText("li:has(em:containsOwn(Publicatiedatum)) > span", overviewPage);
    }

    /**
     * Parse source form type of publication from overview page.
     *
     * @param overviewPage
     *         overview page to be parsed
     *
     * @return String or Null
     */
    private static String parsePublicationSourceFormType(final Document overviewPage) {
        // we gets it from overview page, because the tender page ("Publicatie" tab) can have no information
        // (e.g. https://www.tenderned.nl/tenderned-web/500.xhtml?currentMenu=aankondigingen&cid=182559)
        return JsoupUtils.selectText("li:has(em:containsOwn(Type publicatie)) > span", overviewPage);

        /* Second way (not so much reliable) to get publication source form type from tender page ("Publicatie" tab):
        // only some publications has source form type in "Publicatie" tab (selector is "h1 > span.title"), but almost
        // all publications has it in title
        Elements formTitles = page.select("div[id='content'] > h1");
        assert formTitles.size() == 1;
        String sourceFormTypeAndTitleString = formTitles.get(0).text();
        // publication source form type is the text before first colon (then title is presented and there can be colon)
        return sourceFormTypeAndTitleString.substring(0, sourceFormTypeAndTitleString.indexOf(':')).trim();
        */
    }

    /**
     * Parse source tender ID of publication from overview page.
     *
     * @param overviewPage
     *         overview page to be parsed
     *
     * @return String or Null
     */
    private static String parsePublicationSourceTenderId(final Document overviewPage) {
        return JsoupUtils.selectText("li:has(em:containsOwn(TenderNed-kenmerk)) > span", overviewPage);
    }

    /**
     * Gets type of form for given raw tender publication text.
     *
     * @param sourceFormType
     *         raw tender publication text
     *
     * @return type of form for provided raw tender publication text
     */
    private static PublicationFormType getFormType(final String sourceFormType) {
        if (sourceFormType.equals("Aankondiging van een opdracht")) {
            return PublicationFormType.CONTRACT_NOTICE;
        } else if (sourceFormType.equals("Aankondiging van een gegunde opdracht")) {
            return PublicationFormType.CONTRACT_AWARD;
        } else {
            return PublicationFormType.OTHER;
        }
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "NL";
    }

    @Override
    protected final List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }
}
