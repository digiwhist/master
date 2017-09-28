package eu.digiwhist.worker.uk.parsed;

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
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static eu.dl.worker.utils.jsoup.JsoupUtils.select;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectText;

/**
 * Tender parser for GOV UK.
 *
 * @author Michal Riha
 */
public class GovUKTenderParser extends BaseDigiwhistTenderParser {
    private static final String VERSION = "2";

    @Override
    public final List<ParsedTender> parse(final RawData rawData) {
        final Document document = Jsoup.parse(rawData.getSourceData(), "", Parser.xmlParser());
        List<ParsedTender> parsedTenders = new ArrayList<>();

        Elements rawTenders = select("FullNotice", document);

        for (Element rawTender : rawTenders) {
            final String sourceId = selectText("Notice Id", rawTender);
            parsedTenders.add(new ParsedTender()
                    .addPublication(new ParsedPublication()
                            .setSource(PublicationSources.UK_GOV)
                            .setIsIncluded(true)
                            .setSourceId(sourceId)
                            .setPublicationDate(selectText("PublishedDate", rawTender))
                            .setVersion(selectText("VersionNumber", rawTender))
                            .setLastUpdate(selectText("LastNotifiableUpdate", rawTender))
                            .setHumanReadableUrl(rawData.getSourceUrl().toString())
                            .setSourceFormType(selectText("status", rawTender))
                            .setBuyerAssignedId(selectText("Identifier", rawTender)))
                    .addPublication(new ParsedPublication()
                            .setSource(PublicationSources.UK_GOV)
                            .setIsIncluded(false)
                            .setMachineReadableUrl(PublicationSources.UK_GOV + "Published/Notice/OCDS/" + sourceId))
                    .addPublication(new ParsedPublication()
                            .setSource(PublicationSources.UK_GOV)
                            .setIsIncluded(false)
                            .setMachineReadableUrl(PublicationSources.UK_GOV + "Published/Notice/" + sourceId))
                    .addPublication(new ParsedPublication()
                            .setSource(PublicationSources.UK_TL)
                            .setIsIncluded(false)
                            .setHumanReadableUrl(PublicationSources.UK_TL + sourceId))
                    .addBuyer(new ParsedBody()
                            .setName(parseBuyerName(rawTender))
                            .setAddress(new ParsedAddress()
                                    .setStreet(selectText("ContactDetails Address1", rawTender) + ", " +
                                            selectText("ContactDetails Address2", rawTender))
                                    .setCity(selectText("ContactDetails Town", rawTender))
                                    .setPostcode(selectText("ContactDetails Postcode", rawTender))
                                    .setCountry(selectText("ContactDetails Country", rawTender))
                                    .setUrl(selectText("ContactDetails WebAddress", rawTender)))
                            .setPhone(selectText("ContactDetails Phone", rawTender))
                            .setEmail(selectText("ContactDetails Email", rawTender)))
                    .setTitle(selectText("Title", rawTender))
                    .setProcedureType(selectText("ProcedureType", rawTender))
                    .setSupplyType(selectText("OjeuContractType", rawTender))
                    .setIsFrameworkAgreement(selectText("IsFrameworkAgreement", rawTender))
                    .setAddressOfImplementation(new ParsedAddress()
                            .setPostcode(setAOIPostcode(rawTender)))
                    .setDescription(selectText("Description", rawTender))
                    .setCpvs(parseCPVs(rawTender))
                    .setFinalPrice(new ParsedPrice()
                            .setMinNetAmount(selectText("ValueLow", rawTender))
                            .setMaxNetAmount(selectText("ValueHigh", rawTender)))
                    .setLots(parseLots(rawTender))
                    .setEstimatedStartDate(selectText("Start", rawTender))
                    .setEstimatedCompletionDate(selectText("End", rawTender))
                    .setBidDeadline(selectText("DeadlineDate", rawTender))
                    .setNationalProcedureType(selectText("AwardDetail AwardedProcedureType", rawTender))

            );
        }

        return parsedTenders;
    }

    /**
     * Parse ID with UNKNOWN scope.
     *
     * @param selector selector
     * @param rawTender element to parse from
     * @return BodyIdentifier or null
     */
    private BodyIdentifier parseUnknownId(final String selector, final Element rawTender) {
        final String buyerId = selectText(selector, rawTender);

        return buyerId == null || buyerId.equals("") ? null : new BodyIdentifier()
                .setId(buyerId)
                .setScope(BodyIdentifier.Scope.UNKNOWN);
    }

    /**
     * Parse buyer name.
     *
     * @param rawTender element to parse from
     *
     * @return String
     */
    private String parseBuyerName(final Element rawTender) {
        String buyerName = selectText("ContactDetails Name", rawTender);

        if (buyerName == null) {
            buyerName = selectText("OrganisationName", rawTender);
        }

        return buyerName;
    }

    /**
     * Parse Lots for tender.
     *
     * @param rawTender tender to parse from
     *
     * @return list of parsed lots.
     */
    private List<ParsedTenderLot> parseLots(final Element rawTender) {
        return Arrays.asList(new ParsedTenderLot()
                .addBid(new ParsedBid()
                        .addBidder(new ParsedBody()
                                .setName(selectText("AwardDetail SupplierName", rawTender))
                                .setAddress(new ParsedAddress()
                                        .setRawAddress(selectText("AwardDetail SupplierAddress", rawTender)))
                                .setContactName(selectText("AwardDetail Contact", rawTender))
                                .addBodyId(parseUnknownId("AwardDetail DunsNumber", rawTender))
                                .setIsSme(selectText("AwardDetail AwardedToSME", rawTender)))
                        .setPrice(new ParsedPrice()
                                .setNetAmount(selectText("AwardDetail Value", rawTender)))
                        .setIsWinning(String.valueOf(true)))
                .setStatus(selectText("Status", rawTender))
                .setEstimatedStartDate(selectText("AwardDetail Startdate", rawTender))
                .setEstimatedCompletionDate(selectText("AwardDetail EndDate", rawTender))
                .setStatus(selectText("Status", rawTender))
                .setAwardDecisionDate(selectText("AwardDetail AwardedDate", rawTender)));
    }

    /**
     * Parse postcode of address of implementation.
     *
     * @param rawTender tender to parse from
     *
     * @return postcode of address of implementation
     */
    private String setAOIPostcode(final Element rawTender) {
        List<Element> postcodes = select("Postcode", rawTender);

        if (postcodes != null && postcodes.size() > 1) {
            return postcodes.get(1).text();
        }

        return null;
    }

    /**
     * Parse CPVs of tender.
     *
     * @param rawTender tender to parse from
     *
     * @return list of parsed CPVs
     */
    private List<ParsedCPV> parseCPVs(final Element rawTender) {
        List<ParsedCPV> parsedCPVs = new ArrayList<>();

        List<Element> parsedCpvElements = select("CpvCodes string", rawTender);
        for (Element parsedCpvElement : parsedCpvElements) {
            parsedCPVs.add(new ParsedCPV()
                    .setCode(parsedCpvElement.text())
                    .setIsMain(String.valueOf(false)));
        }
        parsedCPVs.add(new ParsedCPV()
                .setCode(selectText("CpvCodes CpvCode Code", rawTender))
                .setIsMain(String.valueOf(true)));

        return parsedCPVs.isEmpty() ? null : parsedCPVs;
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "UK";
    }
}
