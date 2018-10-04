package eu.datlab.worker.ge.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.ge.SPATenderUtils;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.DocumentType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedDocument;
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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Tender parser for Georgia.
 *
 * @author Marek Mikes
 */
public class SPATenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1";

    private static final String BODY_ID_TITLE_ON_SUBJECT_SNIPPET = "Identification code";
    private static final String COUNTRY_TITLE_ON_SUBJECT_SNIPPET = "Country";
    private static final String CITY_TITLE_ON_SUBJECT_SNIPPET = "City/town/village";
    private static final String STREET_TITLE_ON_SUBJECT_SNIPPET = "Address";
    private static final String PHONE_TITLE_ON_SUBJECT_SNIPPET = "Phone";
    private static final String EMAIL_TITLE_ON_SUBJECT_SNIPPET = "E-Mail";
    private static final String URL_TITLE_ON_SUBJECT_SNIPPET = "Web address";
    private static final String CONTACT_NAME_SELECTOR_ON_SUBJECT_SNIPPET =
            "div[id='profile_dialog'] > div > table > tbody[id='c'] > tr > td > strong";

    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        ParsedTender parsedTender = new ParsedTender();

        // get main snippet
        final Document mainSnippet = Jsoup.parse(rawTender.getSourceData());

        @SuppressWarnings("unchecked") final HashMap<String, String> pageSnippetMap =
                (HashMap<String, String>) rawTender.getMetaData().get("snippets");

        // get chronology snippet
        final Document chronologySnippet = Jsoup.parse(pageSnippetMap.get("CHANGELOG"));

        // get documentation snippet
        final Document documentationSnippet = Jsoup.parse(pageSnippetMap.get("DOCUMENTATION"));

        // get offers snippet
        final Document offersSnippet = Jsoup.parse(pageSnippetMap.get("OFFERS"));

        // get result snippet
        final Document resultSnippet = Jsoup.parse(pageSnippetMap.get("RESULT"));

        @SuppressWarnings("unchecked") final HashMap<String, String> subjectSnippetMap =
                (HashMap<String, String>) rawTender.getMetaData().get("subjects");

        final String buyerNameTitleRegex = "(?i)Procuring entities|ადმინისტრირებას უწევს";

        // get buyer snippet
        Elements buyerLinkElements = JsoupUtils.select("div[id='print_area'] > table > tbody > tr:has(td:matchesOwn("
            + buyerNameTitleRegex + ")) > td:nth-child(2) > a", mainSnippet);
        assert buyerLinkElements.size() == 1;
        Element buyerLinkElement = buyerLinkElements.get(0);
        final Document buyerSnippet = Jsoup.parse(subjectSnippetMap.get(SPATenderUtils.getSubjectId(buyerLinkElement)));

        // get to know whether all prices are with or without VAT
        final String vatInfo = getFromTableFromMainSnippet("Bid should be submitted", mainSnippet);
        assert vatInfo.equals("Including VAT") || vatInfo.equals("Excluding VAT");
        final boolean includingVat = "Including VAT".equals(vatInfo);

        // get lot estimated start date and lot estimated completion date
        String lotEstimatedStartDate = null;
        String lotEstimatedCompletionDate = null;
        final Element winnerInfoElement = getWinnerInfoElementFrom(resultSnippet);
        if (winnerInfoElement != null) {
            final String winnerInfoElementOwnText = winnerInfoElement.ownText();
            final String estimatedDatesTitle = "Contract validity:";
            assert winnerInfoElementOwnText.contains(estimatedDatesTitle);
            final String startAndCompletionDatesString = winnerInfoElementOwnText.substring(
                    winnerInfoElementOwnText.indexOf(estimatedDatesTitle) + estimatedDatesTitle.length());
            assert startAndCompletionDatesString.contains("-")
                    : "Dash should separate estimated start and estimated completion dates!";
            final String[] startAndCompletionDates = startAndCompletionDatesString.split("-");
            if (startAndCompletionDates.length == 2) {
                lotEstimatedStartDate = startAndCompletionDates[0].trim();
                lotEstimatedCompletionDate = startAndCompletionDates[1].trim();
            } else {
                // e.g. http://tenders.procurement.gov.ge/public/?go=74190&lang=en
                assert startAndCompletionDates.length == 1;
                lotEstimatedStartDate = startAndCompletionDates[0].trim();
            }
        }

        String procedureType = getFromTableFromMainSnippet("Tender type", mainSnippet);

        parsedTender
                .setProcedureType(procedureType)
                .setNationalProcedureType(procedureType)
                .setPublications(parsePublications(mainSnippet, chronologySnippet, rawTender.getSourceUrl().toString()))
                .addLot(new ParsedTenderLot()
                        .setStatus(getFromTableFromMainSnippet("Tender proceeding status", mainSnippet))
                        .setBids(parseBids(offersSnippet, resultSnippet, subjectSnippetMap, includingVat))
                        .setEstimatedStartDate(lotEstimatedStartDate)
                        .setEstimatedCompletionDate(lotEstimatedCompletionDate))
                .addBuyer(new ParsedBody()
                        .setName(getFromTableFromMainSnippet(buyerNameTitleRegex, mainSnippet))
                        .setBuyerType(JsoupUtils.selectText("div[id='profile_dialog'] > div > table > tbody > " +
                                "tr:has(td > strong:containsOwn(Procuring Entity)) > td:nth-child(2) > label",
                            buyerSnippet))
                        .addBodyId(new BodyIdentifier()
                                .setId(getFromTableFromSubjectSnippet(BODY_ID_TITLE_ON_SUBJECT_SNIPPET, buyerSnippet)))
                        .setAddress(new ParsedAddress()
                                .setCountry(getFromTableFromSubjectSnippet(COUNTRY_TITLE_ON_SUBJECT_SNIPPET,
                                        buyerSnippet))
                                .setCity(getFromTableFromSubjectSnippet(CITY_TITLE_ON_SUBJECT_SNIPPET, buyerSnippet))
                                .setStreet(getFromTableFromSubjectSnippet(STREET_TITLE_ON_SUBJECT_SNIPPET,
                                        buyerSnippet))
                                .setUrl(getFromTableFromSubjectSnippet(URL_TITLE_ON_SUBJECT_SNIPPET, buyerSnippet)))
                        .setPhone(getFromTableFromSubjectSnippet(PHONE_TITLE_ON_SUBJECT_SNIPPET, buyerSnippet))
                        .setEmail(getFromTableFromSubjectSnippet(EMAIL_TITLE_ON_SUBJECT_SNIPPET, buyerSnippet))
                        .setContactName(JsoupUtils.selectText(CONTACT_NAME_SELECTOR_ON_SUBJECT_SNIPPET, buyerSnippet)))
                .setBidDeadline(getFromTableFromMainSnippet("Deadline for bid submission", mainSnippet))
                .setEstimatedPrice(parseTenderEstimatedPrice(mainSnippet, includingVat))
                .setCpvs(parseTenderCpvs(mainSnippet))
                .setDescription(JsoupUtils.selectText("div[id='print_area'] > table > tbody > tr > td > div.blabla",
                        mainSnippet))
                .setDeposits(getFromTableFromMainSnippet("Guarantee amount", mainSnippet))
                .setAwardDeadlineDuration(parseAwardDeadlineDuration(mainSnippet))
                .setDocuments(parseTenderDocuments(documentationSnippet, resultSnippet));

        final Elements updateElements = getHighlightElements("Contract modification", resultSnippet);
        if (updateElements != null && !updateElements.isEmpty()) {
                Elements updates = updateElements.get(0).select("table > tbody > tr > td:nth-child(1)");
                updates.forEach(n -> {
                    Matcher m = Pattern.compile("Contract validity:"
                        + " ?(?<start>\\d{2}.\\d{2}.\\d{4})? - (?<end>\\d{2}.\\d{2}.\\d{4})?").matcher(n.text());
                    if (m.find()) {
                        String start = m.group("start");
                        if (start != null) {
                            parsedTender.setEstimatedStartDate(start);
                        }
                        String end = m.group("end");
                        if (end != null) {
                            parsedTender.setEstimatedCompletionDate(end);
                        }
                    }
                });
        }

        return Arrays.asList(parsedTender);
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    /**
     * Gets string from table (has two columns and no border) which is in main snippet. The string follows after its
     * title.
     *
     * @param title
     *         title of the string
     * @param mainSnippet
     *         main snippet to be parsed
     *
     * @return string from table which is in main snippet. The string follows after its title.
     */
    private String getFromTableFromMainSnippet(final String title, final Document mainSnippet) {
        return JsoupUtils.selectText(
                "div[id='print_area'] > table > tbody > tr:has(td:matchesOwn(" + title + ")) > td:nth-child(2)",
                mainSnippet);
    }

    /**
     * Gets string from table (has two columns and no border) which is in buyer snippet. The string follows after its
     * title.
     *
     * @param title
     *         title of the string
     * @param subjectSnippet
     *         subject snippet to be parsed
     *
     * @return string from table which is in buyer snippet. The string follows after its title.
     */
    private String getFromTableFromSubjectSnippet(final String title, final Document subjectSnippet) {
        return JsoupUtils.selectText("div[id='profile_dialog'] > div > table > tbody > tr:has(td:containsOwn(" + title +
                        ")) > td:nth-child(2)",
                subjectSnippet);
    }

    /**
     * Parse tender estimated price value from main snippet.
     *
     * @param mainSnippet
     *         main snippet to be parsed
     * @param includingVat
     *         flag which says whether price including VAT or not
     *
     * @return tender estimated price or Null
     */
    private ParsedPrice parseTenderEstimatedPrice(final Document mainSnippet, final boolean includingVat) {
        final String priceAndCurrencyString = getFromTableFromMainSnippet("Estimated value of procurement",
                mainSnippet);

        if (priceAndCurrencyString == null) {
            return null;
        }

        final String[] priceAndCurrency = priceAndCurrencyString.split(" ");
        assert priceAndCurrency.length == 2;

        final ParsedPrice estimatedPrice = includingVat
                ? new ParsedPrice().setAmountWithVat(priceAndCurrency[0])
                : new ParsedPrice().setNetAmount(priceAndCurrency[0]);
        return estimatedPrice.setCurrency(priceAndCurrency[1]);
    }

    /**
     * Parse tender main CPVs from main snippet.
     *
     * @param mainSnippet
     *         main snippet to be parsed
     *
     * @return list of parsed CPVs or Null
     */
    private List<ParsedCPV> parseTenderCpvs(final Document mainSnippet) {
        List<ParsedCPV> cpvs = new ArrayList<>();

        final String mainCpvAndDescriptionString = getFromTableFromMainSnippet("Procuring category", mainSnippet);
        if (mainCpvAndDescriptionString != null && !mainCpvAndDescriptionString.isEmpty()) {
            assert mainCpvAndDescriptionString.indexOf('-') != -1;
            cpvs.add(new ParsedCPV()
                    .setIsMain(Boolean.TRUE.toString())
                    .setCode(mainCpvAndDescriptionString.substring(0, mainCpvAndDescriptionString.indexOf('-'))
                            .trim()));
        }

        Elements nonMainCpvAndDescriptionElements = JsoupUtils.select(
                "div[id='print_area'] > table > tbody > tr > td > div > ul > li", mainSnippet);
        for (Element nonMainCpvAndDescriptionElement : nonMainCpvAndDescriptionElements) {
            final String nonMainCpvAndDescription = nonMainCpvAndDescriptionElement.ownText();
            assert nonMainCpvAndDescription.indexOf('-') != -1;
            cpvs.add(new ParsedCPV()
                    .setIsMain(Boolean.FALSE.toString())
                    .setCode(nonMainCpvAndDescription.substring(0, nonMainCpvAndDescription.indexOf('-')).trim()));
        }

        return cpvs.isEmpty() ? null : cpvs;
    }

    /**
     * Parses tender publications.
     *
     * @param mainSnippet
     *         main snippet to be parsed
     * @param chronologySnippet
     *         chronology snippet to be parsed
     * @param humanReadableUrl
     *            the human readable url
     *
     * @return parsed main publication
     */
    private List<ParsedPublication> parsePublications(final Document mainSnippet, final Document chronologySnippet,
                                                      final String humanReadableUrl) {
        List<ParsedPublication> publications = new ArrayList<>();

        Elements chronologyRows = JsoupUtils.select("table > tbody > tr", chronologySnippet);
        assert !chronologyRows.isEmpty() : "At least one row containing actual state of tender should be in the table";
        for (Element chronologyRow : chronologyRows) {
            final String publicationDateTime = JsoupUtils.selectText("td:nth-child(1)", chronologyRow);
            assert publicationDateTime.contains(" ")
                    && publicationDateTime.indexOf(" ") == publicationDateTime.lastIndexOf(" ");
            publications.add(new ParsedPublication()
                    .setIsIncluded(false)
                    .setSource(PublicationSources.GE_SPA)
                    .setPublicationDate(publicationDateTime.split(" ")[0])
                    .setSourceFormType(JsoupUtils.selectText("td:nth-child(2)", chronologyRow)));
        }

        // update first publication which is included
        publications.get(0)
                .setIsIncluded(true)
                .setSourceTenderId(getFromTableFromMainSnippet("Tender Registration Number", mainSnippet))
                .setHumanReadableUrl(humanReadableUrl);

        return publications;
    }

    /**
     * Parses award deadline duration.
     *
     * @param mainSnippet
     *         main snippet to be parsed
     *
     * @return award deadline duration
     */
    private String parseAwardDeadlineDuration(final Document mainSnippet) {
        final String awardDeadlineDuration = getFromTableFromMainSnippet("Guarantee validity period", mainSnippet);

        if (awardDeadlineDuration == null || awardDeadlineDuration.equals("Day")) {
            // e.g. http://tenders.procurement.gov.ge/public/?go=175232&lang=en
            return null;
        }

        assert awardDeadlineDuration.indexOf(' ') == awardDeadlineDuration.lastIndexOf(' ')
                && awardDeadlineDuration.endsWith(" Day");
        return awardDeadlineDuration.split(" ")[0];
    }

    /**
     * Parses documents.
     *
     * @param documentationSnippet
     *         documentation snippet to be parsed
     * @param resultSnippet
     *         snippet where result is
     *
     * @return non-empty list of parsed documents or null
     */
    private List<ParsedDocument> parseTenderDocuments(final Document documentationSnippet,
                                                      final Document resultSnippet) {
        List<ParsedDocument> documents = new ArrayList<>();

        // documents from 'Tender documentation' tab
        documents.addAll(parseDocumentsFromTable(JsoupUtils.selectFirst("table[id='tender_docs']",
            documentationSnippet)));
        
        // documents from 'Result' tab
        Element resultSnippetContext = JsoupUtils.selectFirst("div[id='agency_docs']", resultSnippet);

        documents.addAll(parseDocumentsFromTable(JsoupUtils.selectFirst("table:has(thead:contains(Document))",
            resultSnippetContext)));


        final Elements contractAgreementLinkElements = getHighlightElements("Agreement", resultSnippet);
        assert contractAgreementLinkElements.size() <= 1;

        if (!contractAgreementLinkElements.isEmpty()) {
            documents.add(new ParsedDocument()
                .setType(DocumentType.CONTRACTOR_AGREEMENT.name())
                .setUrl(parseAbsoluteDocumentUrl(JsoupUtils.selectFirst("table > tbody > tr > td:eq(1) > a",
                    contractAgreementLinkElements.get(0)))));
        }


        final Elements contractModificationElements = getHighlightElements("Contract modification", resultSnippet);
        assert contractModificationElements.size() <= 1;

        if (!contractModificationElements.isEmpty()) {
            documents.add(new ParsedDocument()
                .setUrl(parseAbsoluteDocumentUrl(JsoupUtils.selectFirst("table > tbody > tr > td:eq(1) > a",
                    contractModificationElements.get(0)))));
        }

        return documents.isEmpty() ? null : documents;
    }

    /**
     * @param anchor
     *      anchor whose relative link leads to the document
     * @return absolute link to the document
     */
    private String parseAbsoluteDocumentUrl(final Element anchor) {
        return PublicationSources.GE_SPA + "/public/" + JsoupUtils.selectAttribute("href", anchor);
    }

    /**
     * @param table
     *      table that includes documents
     * @return list of documents if some exist or empty list
     */
    private List<ParsedDocument> parseDocumentsFromTable(final Element table) {
        final Elements rows = JsoupUtils.select("tbody > tr", table);
        if (rows == null || rows.isEmpty() || rows.get(0).text().contains("No documents attached")) {
            return Collections.emptyList();
        }

        List<ParsedDocument> documents = new ArrayList<>();
        String separator = "::";
        rows.forEach(r -> {
            final String dateTimeAndAuthor = JsoupUtils.selectText("td:nth-child(3)", r);

            assert dateTimeAndAuthor.contains(separator)
                && dateTimeAndAuthor.indexOf(separator) == dateTimeAndAuthor.lastIndexOf(separator);

            documents.add(new ParsedDocument()
                .setTitle(JsoupUtils.selectText("td:nth-child(2)", r))
                .setUrl(parseAbsoluteDocumentUrl(JsoupUtils.selectFirst("td:nth-child(2) > a", r)))
                .setPublicationDateTime(dateTimeAndAuthor.split(separator)[0].trim()));
        });
        
        return documents;
    }

    /**
     * Parses bids of lot.
     *
     * @param offersSnippet
     *         snippet where offers are
     * @param resultSnippet
     *         snippet where result is
     * @param subjectSnippetMap
     *         snippet map where all subjects are
     * @param includingVat
     *         flag which says whether price including VAT or not
     *
     * @return list of lot bids
     */
    private List<ParsedBid> parseBids(final Document offersSnippet, final Document resultSnippet,
                                      final HashMap<String, String> subjectSnippetMap, final boolean includingVat) {
        Elements bidderRows = JsoupUtils.select(
                "table:has(thead > tr > td:nth-child(2):containsOwn(Last Offer)) > tbody > tr", offersSnippet);

        if (bidderRows.isEmpty()) {
            return null;
        }

        final Element winnerInfoElement = getWinnerInfoElementFrom(resultSnippet);

        String winnerId = SPATenderUtils.getSubjectId(JsoupUtils.selectFirst("a", winnerInfoElement));

        // get disqualified bidders from result snippet
        final Elements disqualifiedBidderRows = JsoupUtils.select(
                "div[id='agency_docs'] > div > table:has(tr:containsOwn(Disqualification)) > tbody > tr",
                resultSnippet);



        List<ParsedBid> bids = new ArrayList<>();
        for (Element bidderRow : bidderRows) {
            // parse bidder info from the row
            ParsedBid bid = new ParsedBid();

            final String amount = JsoupUtils.selectText("td:nth-child(2) > strong", bidderRow);
            if (includingVat) {
                bid.setPrice(new ParsedPrice()
                        .setAmountWithVat(amount));
            } else {
                bid.setPrice(new ParsedPrice()
                        .setNetAmount(amount));
            }

            // get bidder ID
            Element bidderLinkElement = JsoupUtils.selectFirst("td:nth-child(1) > a", bidderRow);
            final String bidderId = SPATenderUtils.getSubjectId(bidderLinkElement);

            // parse bidder info from bidder detail snippet
            final Document bidderSnippet = Jsoup.parse(subjectSnippetMap.get(bidderId));
            bid.addBidder(new ParsedBody()
                .setName(JsoupUtils.selectText("table > tbody > tr > td:eq(1) > strong", bidderSnippet))
                .addBodyId(new BodyIdentifier()
                    .setId(getFromTableFromSubjectSnippet(BODY_ID_TITLE_ON_SUBJECT_SNIPPET, bidderSnippet)))
                .setAddress(new ParsedAddress()
                    .setCountry(getFromTableFromSubjectSnippet(COUNTRY_TITLE_ON_SUBJECT_SNIPPET, bidderSnippet))
                    .setCity(getFromTableFromSubjectSnippet(CITY_TITLE_ON_SUBJECT_SNIPPET, bidderSnippet))
                    .setStreet(getFromTableFromSubjectSnippet(STREET_TITLE_ON_SUBJECT_SNIPPET, bidderSnippet))
                    .setUrl(getFromTableFromSubjectSnippet(URL_TITLE_ON_SUBJECT_SNIPPET, bidderSnippet)))
                .setPhone(getFromTableFromSubjectSnippet(PHONE_TITLE_ON_SUBJECT_SNIPPET, bidderSnippet))
                .setEmail(getFromTableFromSubjectSnippet(EMAIL_TITLE_ON_SUBJECT_SNIPPET, bidderSnippet))
                .setContactName(JsoupUtils.selectText(CONTACT_NAME_SELECTOR_ON_SUBJECT_SNIPPET, bidderSnippet)));
            
            // parse whether the bid won
            bid.setIsWinning(Boolean.toString(bidderId.equals(winnerId)));

            List<Element> matchedDisqualifiedBidderRows = disqualifiedBidderRows
                    .stream()
                    .filter(r -> bid.getBidders().get(0).getName().equals(
                            JsoupUtils.selectText("td:nth-child(2) > strong", r)))
                    .collect(Collectors.toList());
            if (!matchedDisqualifiedBidderRows.isEmpty()) {
                assert matchedDisqualifiedBidderRows.size() == 1;
                Element matchedDisqualifiedBidderRow = matchedDisqualifiedBidderRows.get(0);
                bid
                        .setIsDisqualified(Boolean.TRUE.toString())
                        .setDisqualificationReason(JsoupUtils.selectText("td:nth-child(3)",
                                matchedDisqualifiedBidderRow));
            }

            bids.add(bid);
        }
        return bids;
    }

    /**
     * Gets element containing information about winner from result snippet.
     *
     * @param resultSnippet
     *         snippet where result is
     *
     * @return element containing information about winner from result snippet or null.
     */
    private Element getWinnerInfoElementFrom(final Document resultSnippet) {
        final Elements winnerInfoElements = getHighlightElements("Agreement", resultSnippet);
        assert winnerInfoElements.size() <= 1;

        return winnerInfoElements.isEmpty()
            ? null : JsoupUtils.selectFirst("table > tbody > tr > td:nth-child(1)", winnerInfoElements.get(0));
    }

    /**
     * Selects all highlight elements with the given title.
     *
     * @param title
     *      title of hte highlight element
     * @param context
     *      context to be searched     
     * @return list of elements or null if the context is null
     */
    private Elements getHighlightElements(final String title, final Element context) {
        return JsoupUtils.select("div.ui-state-highlight:has(*:containsOwn(" + title + "))", context);
    }


    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "GE";
    }

}
