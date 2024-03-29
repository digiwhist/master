package eu.datlab.worker.ie.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.TextNode;
import org.jsoup.select.Elements;

import static eu.datlab.worker.ie.ETendersTenderConstants.SYSTEM_ID_COLUMN_METADATA_KEY;
import static eu.datlab.worker.ie.ETendersTenderConstants.TENDER_REFERENCE_COLUMN_METADATA_KEY;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by michalriha on 09/05/2017.
 */
public class ETendersTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    /**
     * This invisible character appears after year in datetime.
     */
    private static final Character CURIOUS_WHITE_SPACE = (char) 160;

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "IE";
    }

    @Override
    public final List<ParsedTender> parse(final RawData raw) {
        final Element document = Jsoup.parse(raw.getSourceData()).select("td.pagebg").first();

        if (document == null || document.select("span:matchesOwn((General error|Access denied))").first() != null) {
            logger.info("No tender data.");
            throw new UnrecoverableException("No tender data to be parsed on message with Id: " + raw.getId());
        }

        // Parse Description and SupplyType
        final List<String> descriptionAndSupplyType = selectNodeTexts("td.tblcell:contains(description)", document);
        String description = null;
        String supplyType = null;

        if (descriptionAndSupplyType != null && descriptionAndSupplyType.size() > 1) {
            description = descriptionAndSupplyType.get(0) + "\n" + descriptionAndSupplyType.get(1);
            supplyType = descriptionAndSupplyType.get(descriptionAndSupplyType.size() - 1);
        }

        // Parse Buyer
        final List<String> buyer = selectNodeTexts("td.tblcell:has(span.celllbl:containsOwn(Contracting authority))",
                document);
        String buyerName = null;
        String buyerAddress = null;

        // check for duplicate last values
        if (buyer != null && buyer.size() > 4 && buyer.get(buyer.size() - 1).equals(buyer.get(buyer.size() - 2))) {
            buyer.remove(buyer.size() - 1);
        }

        if (buyer != null && buyer.size() > 3) {
            buyerName = buyer.get(0);
            if(buyer.size() > 4){
                buyerAddress = buyer.get(buyer.size() - 4) + " ";
            } else {
                buyerAddress = "";
            }
            buyerAddress += buyer.get(buyer.size() - 3) + " " + buyer.get(buyer.size() - 2) + " " + buyer.get(buyer.size() - 1);
        }

        // additional buyer info might be in profile view, which is downloaded as attachment
        final HashMap<String, String> additionalUrls = (HashMap<String, String>) raw.getMetaData()
                .get("additionalUrls");
        final Element profileView = Jsoup.parse(additionalUrls.values().iterator().next());

        final String bidDeadline = JsoupUtils.selectOwnText("td.tblcell:contains(Response deadline)", document);
        String title = JsoupUtils.selectText("td.tblcell:has(*:containsOwn(Short description))", document);
        if(title != null && !title.isEmpty()) {
            title = title.split(" Detailed description")[0].replace("Short description ", "");
        }

        return Collections.singletonList(new ParsedTender()
                .setDescription(description)
                .setSupplyType(supplyType)
                .addBuyer(new ParsedBody()
                        .setName(buyerName)
                        .addBodyId(new BodyIdentifier()
                                .setId(JsoupUtils.selectOwnText(
                                        "div.span4 div.span12:has(i:containsOwn(Organisation no (or VAT)))",
                                        profileView))
                                .setScope(BodyIdentifier.Scope.IE)
                                .setType(BodyIdentifier.Type.ORGANIZATION_ID))
                        .setBuyerType(JsoupUtils.selectOwnText("div.span10:contains(Type of the contracting authority) ul li",
                                profileView))
                        .setEmail(JsoupUtils.selectOwnText("div.span4 div.span12:has(i.icon-envelope-alt) a",
                                profileView))
                        .setAddress(new ParsedAddress()
                                .setRawAddress(buyerAddress))
                        .setContactName(JsoupUtils.selectOwnText("div.span4 div.span12:has(i.icon-user)", profileView))
                        .addMainActivity(JsoupUtils.selectOwnText("div.span10:contains(Main activity) ul li",
                                profileView)))
                .setBidDeadline(bidDeadline)
                .setCpvs(parseCPVs(document))
                .setPublications(parsePublications(document, raw.getSourceUrl().toString(), raw.getMetaData()))
                .setTitle(title)
                .setLots(parseLots(document)));
    }

    /**
     * Parse lots.
     *
     * @param document document to parse from
     *
     * @return List<ParsedTenderLot> or null
     */
    private List<ParsedTenderLot> parseLots(final Element document) {
        final List<ParsedTenderLot> parsedLots = new ArrayList<>();
        Elements lots = JsoupUtils.select("td.tblcell:contains(Awarded supplier) > table > tr", document);
        if(lots != null && !lots.isEmpty()){
            // increment by 2, every first line is title, every second is bodyId
            for (int position = 0; position < lots.size(); position = position + 2) {
                parsedLots.add(new ParsedTenderLot()
                        .addBid(new ParsedBid()
                                .addBidder(new ParsedBody()
                                        .setName(lots.get(position).text())
                                        .addBodyId(!(position + 1 < lots.size()) ? null : new BodyIdentifier()
                                                .setId(lots.get(position + 1).text())
                                                .setScope(BodyIdentifier.Scope.IE)
                                                .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                        )
                                )
                        )
                );
            }
        } else {
            lots = JsoupUtils.select("table:has(td.tblcollbl:containsOwn(Lots)) ~ div", document);
            if(lots != null && !lots.isEmpty()){
                for(int i = 0; i < lots.size(); i++){
                    ParsedTenderLot parsedLot = new ParsedTenderLot();
                    String title = JsoupUtils.selectText("span", lots.get(i));
                    parsedLot.setTitle(title);
                    parsedLot.setLotNumber(String.valueOf(i + 1));
                    String address = JsoupUtils.selectText("td:contains(Tender mailing address)", lots.get(i));
                    if(address != null && !address.isEmpty()){
                        address = address.replace("Tender mailing address", "");
                    }
                    parsedLots.add(parsedLot);
                }
            }
        }

        return parsedLots.isEmpty() ? null : parsedLots;
    }

    /**
     * Parse publications.
     *
     * @param document document to parse from
     * @param url      url of publication
     * @param metadata raw tender metadata
     *
     * @return List<ParsedPublication> or null
     */
    private List<ParsedPublication> parsePublications(final Element document, final String url,
                                                      final HashMap<String, Object> metadata) {
        final Elements publications = JsoupUtils.select("tbody:contains(Date of Dispatch) > tr", document);

        final List<ParsedPublication> parsedPublications = new ArrayList<>();

        // Position 0 is title, start on 1.
        for (int position = 1; position < publications.size(); position++) {
            parsedPublications.add(new ParsedPublication()
                    .setSource(PublicationSources.IE_ETENDERS)
                    .setHumanReadableUrl(url)
                    .setIsIncluded(position + 1 == publications.size())
                    .setSourceFormType(JsoupUtils.selectText("td:eq(0)", publications.get(position)))
                    .setPublicationDate(JsoupUtils.selectText("td:eq(1)", publications.get(position))
                            .split(CURIOUS_WHITE_SPACE.toString())[0])
                    .setSourceTenderId(metadata == null ? null : metadata.get(SYSTEM_ID_COLUMN_METADATA_KEY).toString())
                    .setBuyerAssignedId(
                            metadata == null ? null : metadata.get(TENDER_REFERENCE_COLUMN_METADATA_KEY).toString()));
        }

        return parsedPublications.isEmpty() ? null : parsedPublications;
    }

    /**
     * Parse CPVs.
     *
     * @param document document to parse from
     *
     * @return List<ParsedCPV> or null
     */
    private List<ParsedCPV> parseCPVs(final Element document) {
        List<ParsedCPV> cpvs = null;
        final List<String> result = selectNodeTexts("td.tblcell:contains(CPV codes)", document);
        final List<String> mainCpv = selectNodeTexts("td.tblcell:contains(Main CPV code)", document);
        if(mainCpv != null && !mainCpv.isEmpty()){
            cpvs = new ArrayList<>();
            cpvs.add(new ParsedCPV().setCode(mainCpv.get(0)).setIsMain(String.valueOf(true)));
        }
        if(result != null && !result.isEmpty()){
            if(cpvs == null){
                cpvs = new ArrayList<>();
            }
            cpvs.addAll(result.stream().map(cpv -> new ParsedCPV()
                    .setCode(cpv)
                    .setIsMain(String.valueOf(false))).collect(Collectors.toList()));
            if(mainCpv == null || mainCpv.isEmpty()){
                cpvs.get(0).setIsMain(String.valueOf(true));
            }
        }

        return cpvs;
    }

    /**
     * Parse node texts.
     *
     * @param selector selector to parse with
     * @param document document to parse from
     *
     * @return List<String> or null
     */
    private List<String> selectNodeTexts(final String selector, final Element document) {
        final Element preResult = document.select(selector)
                .first();

        if (preResult == null) {
            return null;
        }

        final List<TextNode> result = preResult.textNodes();

        return result.isEmpty() ? null : result.stream().map(TextNode::text).collect(Collectors.toList());
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }
}
