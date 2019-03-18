package eu.datlab.worker.uk.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
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
import java.util.Arrays;
import java.util.List;

import static eu.dl.worker.utils.jsoup.JsoupUtils.select;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectText;

/**
 * Tender parser for GOV UK.
 *
 * @author Michal Riha
 */
public class GovUKTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "2";

    @Override
    public final List<ParsedTender> parse(final RawData rawData) {
        final Document document = Jsoup.parse(rawData.getSourceData(), "", Parser.xmlParser());
        List<ParsedTender> parsedTenders = new ArrayList<>();

        Elements rawTenders = select("FullNotice", document);

        for (Element rawTender : rawTenders) {
            final String sourceId = selectText("Notice Id", rawTender);
            String procedureType = selectText("ProcedureType", rawTender);
            String nationalProcedureType = selectText("AwardDetail AwardedProcedureType", rawTender);

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
                            .setName(selectText("OrganisationName", rawTender))
                            .setAddress(new ParsedAddress()
                                    .setStreet(selectText("ContactDetails Address1", rawTender) + ", " +
                                            selectText("ContactDetails Address2", rawTender))
                                    .setCity(selectText("ContactDetails Town", rawTender))
                                    .setPostcode(selectText("ContactDetails Postcode", rawTender))
                                    .setCountry(selectText("ContactDetails Country", rawTender))
                                    .setUrl(selectText("ContactDetails WebAddress", rawTender)))
                            .setPhone(selectText("ContactDetails Phone", rawTender))
                            .setEmail(selectText("ContactDetails Email", rawTender))
                            .setContactName(selectText("ContactDetails Name", rawTender)))
                    .setTitle(selectText("Title", rawTender))
                    .setAwardDecisionDate(selectText("AwardedDate", rawTender))
                    .setProcedureType(procedureType != null ? procedureType : nationalProcedureType)
                    .setIsAwarded(selectText("Status", rawTender))
                    .setNationalProcedureType(nationalProcedureType)
                    .setSupplyType(selectText("OjeuContractType", rawTender))
                    .setIsFrameworkAgreement(selectText("IsFrameworkAgreement", rawTender))
                    .setAddressOfImplementation(new ParsedAddress()
                            .setPostcode(setAOIPostcode(rawTender)))
                    .setDescription(selectText("Description", rawTender))
                    .setCpvs(parseCPVs(rawTender))
                    .setEstimatedPrice(parseEstimatedPrice(rawTender))
                    .setLots(parseLots(rawTender))
                    .setEstimatedStartDate(selectText("Start", rawTender))
                    .setEstimatedCompletionDate(selectText("End", rawTender))
                    .setBidDeadline(selectText("DeadlineDate", rawTender)));
        }

        return parsedTenders;
    }

    /**
     * @param rawTender element to parse from
     * @return estimated price or null
     */
    private ParsedPrice parseEstimatedPrice(final Element rawTender) {
        String low = selectText("ValueLow", rawTender);
        String high = selectText("ValueHigh", rawTender);

        if (low == null && high == null) {
            return null;
        } else {
            if (high == null) {
                return new ParsedPrice().setNetAmount(low);
            } else if (low == null || low.equals(high)) {
                return new ParsedPrice().setNetAmount(high);
            } else {
                return new ParsedPrice()
                        .setMinNetAmount(low)
                        .setNetAmount(high)
                        .setMaxNetAmount(high);
            }
        }
    }

    /**
     * Parse ID with UNKNOWN scope.
     *
     * @param selector  selector
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
     * Parse Lots for tender.
     *
     * @param rawTender tender to parse from
     * @return list of parsed lots.
     */
    private List<ParsedTenderLot> parseLots(final Element rawTender) {
        Element awardNode = JsoupUtils.selectFirst("AwardDetail", rawTender);
        ParsedBid bid = null;
        if (awardNode != null) {
            bid = new ParsedBid()
                    .addBidder(new ParsedBody()
                            .setName(selectText("SupplierName", awardNode))
                            .setAddress(new ParsedAddress().setRawAddress(selectText("SupplierAddress", awardNode)))
                            .setContactName(selectText("Contact", awardNode))
                            .addBodyId(parseUnknownId("DunsNumber", awardNode))
                            .setIsSme(selectText("AwardedToSME", awardNode)))
                    .setPrice(new ParsedPrice().setNetAmount(selectText("Value", awardNode)))
                    .setIsWinning(String.valueOf(true));
        }

        return Arrays.asList(new ParsedTenderLot()
                .addBid(bid)
                .setStatus(selectText("Status", rawTender))
                .setEstimatedStartDate(selectText("StartDate", awardNode))
                .setEstimatedCompletionDate(selectText("EndDate", awardNode))
                .setAwardDecisionDate(selectText("AwardedDate", awardNode)));
    }

    /**
     * Parse postcode of address of implementation.
     *
     * @param rawTender tender to parse from
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
     * @return list of parsed CPVs
     */
    private List<ParsedCPV> parseCPVs(final Element rawTender) {
        List<ParsedCPV> cpvs = new ArrayList<>();

        List<Element> nodes = select("CpvCodes string, CpvCodes CpvCode Code", rawTender);
        nodes.forEach(n -> {
            cpvs.add(new ParsedCPV()
                    .setCode(n.text())
                    .setIsMain(Boolean.FALSE.toString()));
        });

        return cpvs.isEmpty() ? null : cpvs;
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "UK";
    }
}
