package eu.datlab.worker.za.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Parser.
 */
public final class ETendersTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    @Override
    protected String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "ZA";
    }

    @Override
    public List<ParsedTender> parse(final RawData raw) {
        HashMap<String, Object> metadata = raw.getMetaData();

        // Raw from tenders result form
        final Document resultFormRow = Jsoup.parse(raw.getSourceData());

        // Detail page
        final Document detailPage = metadata.get("detailPage") == null ? null : Jsoup.parse((String) metadata.get("detailPage"));

        // Pages of bidders
        final List<Document> bidderPages = metadata.get("bidders") == null ? null : ((List<String>) metadata.get("bidders"))
                .stream()
                .map(Jsoup::parse)
                .collect(Collectors.toList());

        // Parsing notice information
        final ParsedTender parsedTender = new ParsedTender()
                .setTitle(getElemenWithoutHeader("Bid Description:", detailPage))
                .setIsAwarded(JsoupUtils.selectText("*:containsOwn(Successful bidder)", detailPage))
                .addPublication(new ParsedPublication()
                        .setSource(PublicationSources.ZA_ETENDERS)
                        .setSourceTenderId((String) raw.getMetaData().get("tenderNumber"))
                        .setSourceFormType((String) raw.getMetaData().get("formType"))
                        .setPublicationDate((String) raw.getMetaData().get("publishedDate"))
                        .setHumanReadableUrl(raw.getSourceUrl().toString())
                        .setIsIncluded(true)
                        .setLanguage("ZA")
                )
                .setSupplyType((String) raw.getMetaData().get("supplyType"))
                .setBidDeadline((String) raw.getMetaData().get("closedDate"))
                .addBuyer(new ParsedBody()
                        .setName(getElemenWithoutHeader("Name of Institution:", resultFormRow))
                        .setEmail(getStringFromtText("Email", detailPage))
                        .setContactName(getStringFromtText("Contact Person", detailPage))
                )
                .setBidDeadline(getElemenWithoutHeader("Closing Date / Time:", resultFormRow))
                .setAddressOfImplementation(new ParsedAddress()
                        .setRawAddress(getElemenWithoutHeader("Place where goods, works or services are required:", resultFormRow)));


        // parsing information about bidders from detail page and getting additional data from downloaded bidder pages in metadata
        final Elements biddersElements = JsoupUtils.select("thead:contains(Bidder name) + tbody > tr", detailPage);

        if (biddersElements != null) {
            for (int i = 0; i < biddersElements.size(); i++) {
                final Element bidderElement = biddersElements.get(i);
                final Document bidderPage = bidderPages == null || bidderPages.size() <= i ? null : bidderPages.get(i);

                parsedTender.addLot(new ParsedTenderLot()
                        .setIsAwarded(JsoupUtils.selectText("h2:containsOwn(Successful bidder)", bidderPage))
                        .setEstimatedCompletionDate(JsoupUtils.selectText("p:containsOwn(Contract End Date)", bidderPage))
                        .setBidsCount(parseBidsCount(detailPage))
                        .addBid(bidderPage == null ? null : new ParsedBid()
                                .setIsWinning(Boolean.TRUE.toString())
                                .setPrice(new ParsedPrice()
                                        .setNetAmount(JsoupUtils.selectText("p:containsOwn(Contract Value)", bidderPage))
                                        .setCurrency("ZAR")
                                )
                                .addBidder(new ParsedBody()
                                        .setName(JsoupUtils.selectText("td:eq(0)", bidderElement))
                                        .setContactName(JsoupUtils.selectText("p:containsOwn(Directors Name)", bidderPage)
                                        )
                                )
                                .setIsWinning(Boolean.TRUE.toString())
                        ));
            }

            // Additional parsing, alternative places to look for variables
            if (parsedTender.getTitle() == null) {
                parsedTender.setTitle(JsoupUtils.selectText("h1.page-title", detailPage));
            }
        }

        return new ArrayList<>(Arrays.asList(parsedTender));
    }

    /**
     * Parse bids count.
     *
     * @param detailPage detail page
     * @return String
     */
    private String parseBidsCount(final Document detailPage) {
        String bidsCount = JsoupUtils.selectText("*:containsOwn(Participants as per the Tender)", detailPage);

        if (bidsCount != null) {
            bidsCount = bidsCount
                    .replaceAll("^.*\\(", "")
                    .replaceAll("\\).*", "");

            if (!bidsCount.isEmpty()) {
                return bidsCount;
            }
        }

        return  String.valueOf(JsoupUtils.select("table > tbody > tr", detailPage).size());
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    /**
     * Get element containing selectors text, but remove selector text.
     *
     * @param selector selector
     * @param element  element
     * @return String or null
     */
    private String getElemenWithoutHeader(final String selector, final Element element) {
        if (element == null) {
            return null;
        }

        final String resultWithheader = JsoupUtils.selectText("*:containsOwn(" + selector + ")", element);
        return resultWithheader == null ? null : resultWithheader.replaceAll(".*:", "");
    }

    /**
     * In element with multiple text nodes, find one by selector and return following.
     *
     * @param selector selector
     * @param element  element
     * @return String or null
     */
    private String getStringFromtText(final String selector, final Element element) {
        if (element == null) {
            return null;
        }

        final Element header = JsoupUtils.selectFirst("*:containsOwn(" + selector + ")", element);
        if (header != null) {
            final Elements headerSiblings = header.parent().children();

            boolean nextIsResult = false;
            for (Element sibling : headerSiblings) {
                if (nextIsResult) {
                    return sibling.text();
                }

                if (sibling.text().contains("selector")) {
                    nextIsResult = true;
                }
            }
        }

        return null;
    }
}
