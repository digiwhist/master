package eu.digiwhist.worker.no.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by michalriha on 02/05/2017.
 */
public class DOFFINTenderParser extends BaseDigiwhistTenderParser {
    private static final String VERSION = "1.0";

    @Override
    public final List<ParsedTender> parse(final RawData raw) {
        final Document document = Jsoup.parse(raw.getSourceData(), "", Parser.xmlParser());

        Element buyer = document.select("CONTRACTING_AUTHORITY_VEAT").first();
        if (buyer == null) {
            buyer = document.select("CONTRACTING_BODY").first();
        }

        return Collections.singletonList(new ParsedTender()
                .setPublications(parsePublications(document, raw.getSourceUrl().toString()))
                .addBuyer(new ParsedBody()
                        .setName(JsoupUtils.selectText("OFFICIALNAME", buyer))
                        .addBodyId(new BodyIdentifier()
                                .setId(JsoupUtils.selectText("NATIONALID", buyer))
                                .setType(BodyIdentifier.Type.HEADER_ICO)
                                .setScope(BodyIdentifier.Scope.NO))
                        .setAddress(new ParsedAddress()
                                .setStreet(JsoupUtils.selectText("ADDRESS", buyer))
                                .setCity(JsoupUtils.selectText("TOWN", buyer))
                                .setPostcode(JsoupUtils.selectText("POSTAL_CODE", buyer))
                                .setCountry(JsoupUtils.selectAttribute("COUNTRY", "VALUE", buyer))
                                .setUrl(JsoupUtils.selectText("URL_BUYER", document)))
                        .setContactPoint(JsoupUtils.selectText("ATTENTION, CONTACT_POINT", buyer))
                        .setPhone(JsoupUtils.selectText("PHONE", buyer))
                        .setEmail(JsoupUtils.selectText("E_MAIL", buyer))
                        .setBuyerType(JsoupUtils.selectAttribute("TYPE_OF_CONTRACTING_AUTHORITY, CA_TYPE",
                                "VALUE", buyer))
                        .addMainActivity(JsoupUtils.selectAttribute("TYPE_OF_ACTIVITY", "VALUE", buyer)))
                .addOnBehalfOf(parseOnBehalfOf(document))
                .setTitle(JsoupUtils.selectText("TITLE_CONTRACT, TITLE", document))
                .setSupplyType(parseSupplyType(document))
                .setAddressOfImplementation(parseAddressOfImplementation(document))
                .setDescription(JsoupUtils.selectText("SHORT_CONTRACT_DESCRIPTION", document))
                .addCpv(new ParsedCPV()
                        .setCode(JsoupUtils.selectAttribute("CPV_CODE", "CODE", document))
                        .setIsMain(String.valueOf(true)))
                .setIsCoveredByGpa(JsoupUtils.selectAttribute("CONTRACT_COVERED_GPA", "YES", document))
                .setFinalPrice(parseFinalPrice(document))
                .setProcedureType(JsoupUtils.selectText("TYPE_OF_PROCEDURE_DEF_F15", document))
                .setLots(parseLots(document))
                .setAwardDecisionDate(parseDate("CONTRACT_AWARD_DATE, DATE_CONCLUSION_CONTRACT", document))
                .addNpwpReason(JsoupUtils.selectTagName("REASONS_PROVIDED_PARTICULAR_TENDERER > *", document))
                .setDocumentsLocation(parseDocumentsLocation(document)));
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "NO";
    }

    /**
     * Parse address of implementation.
     *
     * @param document document to parse from
     *
     * @return ParsedAddress or null
     */
    private ParsedAddress parseAddressOfImplementation(final Document document) {
        final ParsedAddress parsedAddress = new ParsedAddress()
                .setRawAddress(JsoupUtils.selectText("LOCATION_NUTS", document))
                .addNuts(JsoupUtils.selectAttribute("NUTS", "CODE", document));

        return parsedAddress.getRawAddress() == null ? null : parsedAddress;
    }

    /**
     * Parse supply type.
     *
     * @param document document to parse from
     *
     * @return String or null
     */
    private String parseSupplyType(final Document document) {
        String supplyType = JsoupUtils.selectAttribute("TYPE_CONTRACT", "VALUE", document);

        if (supplyType == null) {
            supplyType = JsoupUtils.selectAttribute("TYPE_CONTRACT", "CTYPE", document);
        }

        return supplyType;
    }

    /**
     * Parse on behalf of.
     *
     * @param document document to parse from
     *
     * @return ParsedBody or null
     */
    private ParsedBody parseOnBehalfOf(final Document document) {
        final ParsedBody parsedBody = new ParsedBody()
                .setName(JsoupUtils.selectText("PURCHASING_ON_BEHALF OFFICIALNAME", document))
                .addBodyId(new BodyIdentifier()
                        .setId(JsoupUtils.selectText("PURCHASING_ON_BEHALF NATIONALID", document))
                        .setType(BodyIdentifier.Type.HEADER_ICO)
                        .setScope(BodyIdentifier.Scope.NO))
                .setAddress(new ParsedAddress()
                        .setStreet(JsoupUtils.selectText("PURCHASING_ON_BEHALF ADDRESS", document))
                        .setCity(JsoupUtils.selectText("PURCHASING_ON_BEHALF TOWN", document))
                        .setPostcode(JsoupUtils.selectText("PURCHASING_ON_BEHALF POSTAL_CODE", document))
                        .setCountry(JsoupUtils.selectAttribute("PURCHASING_ON_BEHALF COUNTRY", "VALUE",
                                document)));

        return parsedBody.getName() == null ? null : parsedBody;
    }

    /**
     * Parse lots.
     *
     * @param document document to parse from
     *
     * @return List<ParsedTenderLot> or null
     */
    private List<ParsedTenderLot> parseLots(final Document document) {
        final Elements lots = JsoupUtils.select("AWARD_OF_CONTRACT_DEFENCE, OBJECT_DESCR, AWARD_CONTRACT", document);
        final List<ParsedTenderLot> parsedLots = new ArrayList<>();

        int positionOnPage = 1;
        for (Element lot : lots) {
            parsedLots.add(new ParsedTenderLot()
                    .setPositionOnPage(String.valueOf(positionOnPage++))
                    .setLotNumber(JsoupUtils.selectText("LOT_NUMBER, LOT_NO", lot))
                    .setTitle(JsoupUtils.selectText("CONTRACT_TITLE, TITLE", lot))
                    .addBid(new ParsedBid()
                            .addBidder(new ParsedBody()
                                    .setName(JsoupUtils.selectText("OFFICIALNAME", lot))
                                    .addBodyId(new BodyIdentifier()
                                            .setId(JsoupUtils.selectText("NATIONALID", lot))
                                            .setType(BodyIdentifier.Type.HEADER_ICO)
                                            .setScope(BodyIdentifier.Scope.NO))
                                    .setAddress(new ParsedAddress()
                                            .setStreet(JsoupUtils.selectText("ADDRESS", lot))
                                            .setCity(JsoupUtils.selectText("TOWN", lot))
                                            .setPostcode(JsoupUtils.selectText("POSTAL_CODE", lot))
                                            .setCountry(JsoupUtils.selectAttribute("COUNTRY", "VALUE", lot)))
                                    .setEmail(JsoupUtils.selectText("E_MAIL", lot)))
                            .setIsSubcontracted(JsoupUtils.selectText("NO_CONTRACT_LIKELY_SUB_CONTRACTED", lot))
                            .setPrice(parseLotPrice(lot))
                            .setIsWinning(Boolean.TRUE.toString()))
                    .setEstimatedPrice(new ParsedPrice()
                            .setNetAmount(JsoupUtils.selectText("INITIAL_ESTIMATED_TOTAL_VALUE_CONTRACT > VALUE_COST",
                                    lot))
                            .setCurrency(JsoupUtils.selectAttribute("INITIAL_ESTIMATED_TOTAL_VALUE_CONTRACT",
                                    "CURRENCY", lot))));
        }

        return parsedLots;
    }

    /**
     * Parse lots parsed price.
     *
     * @param lot lot to parse from
     *
     * @return ParsedPrice or null
     */
    private ParsedPrice parseLotPrice(final Element lot) {
        final ParsedPrice parsedPrice = new ParsedPrice()
                .setNetAmount(JsoupUtils.selectText("VAL_TOTAL", lot))
                .setCurrency(JsoupUtils.selectAttribute("VAL_TOTAL", "CURRENCY", lot));

        return parsedPrice.getNetAmount() == null ? null : parsedPrice;
    }

    /**
     * Parse final price.
     *
     * @param document document to parse from
     *
     * @return ParsedPrice or null
     */
    private ParsedPrice parseFinalPrice(final Document document) {
        final Element finalPriceWithVAT = JsoupUtils.selectFirst("COSTS_RANGE_AND_CURRENCY_WITH_VAT_RATE", document);

        if (finalPriceWithVAT == null) {
            return null;
        }

        return new ParsedPrice()
                .setAmountWithVat(JsoupUtils.selectText("VALUE_COST", finalPriceWithVAT))
                .setCurrency(JsoupUtils.selectAttribute("CURRENCY", finalPriceWithVAT));
    }

    /**
     * Parse date.
     *
     * @param selector selector to parse with
     * @param document document to parse from
     *
     * @return String or null
     */
    private String parseDate(final String selector, final Document document) {
        final Element dateElement = JsoupUtils.selectFirst(selector, document);

        if (dateElement == null) {
            return null;
        }

        final String day = JsoupUtils.selectText("DAY", dateElement);
        final String month = JsoupUtils.selectText("MONTH", dateElement);
        final String year = JsoupUtils.selectText("YEAR", dateElement);

        if (day == null || month == null || year == null) {
            return dateElement.text();
        } else {
            return day + "-" + month + "-" + year;
        }
    }

    /**
     * Parses documents location. It is the location accessible through folder icon on the result page.
     *
     * @param document parsed document
     *
     * @return documents location or null
     */
    private static ParsedAddress parseDocumentsLocation(final Document document) {
        // the URL is in "URL_DOCUMENT" element when we parse CFT, "EXTERNAL_DOCUMENT_URL" and
        // "EXPRESSION_OF_INTEREST_URL" are in CA.
        // "URL_INFORMATION" in CA represents different URL. See
        // https://www.doffin.no/Eps.Searching/UnsupportedNotice/NoticeXml/2016-665162 , where the element is filled,
        // but there is no documents location.
        String documentsUrl = JsoupUtils.selectText("URL_DOCUMENT, EXTERNAL_DOCUMENT_URL, EXPRESSION_OF_INTEREST_URL",
                document);

        if (documentsUrl == null) {
            return null;
        }

        return new ParsedAddress()
                .setUrl(documentsUrl);
    }

    /**
     * Parse publications.
     *
     * @param document document to parse from
     * @param machineReadableUrl machine readable url for included publication
     *
     * @return List<ParsedPublication> or null
     */
    private List<ParsedPublication> parsePublications(final Document document, final String machineReadableUrl) {
        final List<ParsedPublication> parsedPublications = new ArrayList<>();

        parsedPublications.add(new ParsedPublication()
                .setSource(PublicationSources.NO_DOFFIN)
                .setIsIncluded(true)
                .setSourceId(machineReadableUrl.substring(machineReadableUrl.lastIndexOf('/') + 1))
                .setMachineReadableUrl(machineReadableUrl)
                .setBuyerAssignedId(JsoupUtils.selectText("FILE_REFERENCE_NUMBER, REFERENCE_NUMBER", document))
                .setDispatchDate(parseDate("NOTICE_DISPATCH_DATE, DATE_DISPATCH_NOTICE", document))
                .setSourceFormType(JsoupUtils.selectAttribute("[form]", "form", document)));

        // previous TED publications
        Elements previousPublicationTedIdElements = JsoupUtils.select("NOTICE_NUMBER_OJ", document);
        // publication can contain more links to previous TED publications (
        // https://www.doffin.no/en/Notice/Details/2012-246939 ). We use distinct, because there can be duplicates (
        // https://www.doffin.no/en/Notice/Details/2012-246729 )
        final List<String> previousPublicationTedIds = previousPublicationTedIdElements
                .stream()
                .map(e -> e.text())
                .distinct()
                .collect(Collectors.toList());
        for (final String previousPublicationTedId : previousPublicationTedIds) {
            parsedPublications.add(new ParsedPublication()
                    .setSource(PublicationSources.EU_TED)
                    .setIsIncluded(false)
                    .setSourceId(previousPublicationTedId));
        }

        // previous Doffin publications
        Elements previousPublicationDoffinIdElements = JsoupUtils.select("REFERENCE_DOFFIN", document);
        final List<String> previousPublicationDoffinIds = previousPublicationDoffinIdElements
                .stream()
                .map(e -> e.text())
                .distinct()
                .collect(Collectors.toList());
        for (final String previousPublicationDoffinId : previousPublicationDoffinIds) {
            parsedPublications.add(new ParsedPublication()
                    .setSource(PublicationSources.NO_DOFFIN)
                    .setIsIncluded(false)
                    .setSourceId(previousPublicationDoffinId));
        }

        return parsedPublications;
    }

}
