package eu.datlab.worker.es.parsed;

import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Parses Spanish data from Hacienda source.
 */
public final class HaciendaTenderParser extends BaseDatlabTenderParser {

    private static final String VERSION = "1.0";

    /**
     * Creates xml document from string.
     *
     * @param data xml data in string
     * @return xml document
     */
    private Document stringToXmlDocument(final String data) {
        if (data == null) {
            return null;
        }
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = null;
        org.w3c.dom.Document doc = null;
        try {
            dBuilder = dbFactory.newDocumentBuilder();
            doc = dBuilder.parse(new InputSource(new StringReader(
                    // to avoid "Content is not allowed in prolog"
                    data.trim().replaceFirst("^([\\W]+)<", "<")
            )));
            doc.getDocumentElement().normalize();
        } catch (ParserConfigurationException | IOException | org.xml.sax.SAXException e) {
            e.printStackTrace();
        }
        return doc;
    }

    /**
     * Gets first node (in given Element) with given name.
     *
     * @param entry element to search node in
     * @param name  name of node to search
     * @return node with give name or null if node was not found
     */
    private Node getFirstNodeForName(final Element entry, final String name) {
        if (entry == null) {
            return null;
        }
        NodeList nodes = entry.getElementsByTagName(name);
        if (nodes != null && nodes.getLength() > 0) {
            return nodes.item(0);
        }
        return null;
    }

    /**
     * Gets text from first node with given name (in given Element).
     *
     * @param entry element to search node in
     * @param name  name of node to search
     * @return text from node with given name or null if node was not found
     */
    private String getFirstTextForName(final Element entry, final String name) {
        if (entry == null) {
            return null;
        }
        Node node = getFirstNodeForName(entry, name);
        if (node == null) {
            return null;
        }
        String text = node.getTextContent();
        if (text != null && !text.isEmpty()) {
            return text;
        }
        return null;
    }

    /**
     * Parses price from given node.
     *
     * @param priceNode node, containing data about price
     * @return parsed price
     */
    private ParsedPrice parsePrice(final Node priceNode) {
        if (priceNode == null) {
            return null;
        }
        String currency = null;
        Node tmp = priceNode.getAttributes().getNamedItem("currencyID");
        if (tmp != null) {
            currency = tmp.getTextContent();
        }
        return new ParsedPrice()
                .setNetAmount(priceNode.getTextContent())
                .setCurrency(currency);
    }

    /**
     * Parses CPVs.
     *
     * @param entry element to parse
     * @return list of parsed CPVs or null if no CPVs were found
     */
    private List<ParsedCPV> parseCpvs(final Element entry) {
        List<ParsedCPV> cpvs = new ArrayList<>();
        NodeList cpvNodes = entry.getElementsByTagName("cbc:ItemClassificationCode");
        for (int j = 0; j < cpvNodes.getLength(); j++) {
            Node item = cpvNodes.item(j);
            if (item != null && item.getTextContent() != null && !item.getTextContent().isEmpty()) {
                // only first cpv has isMain true
                cpvs.add(new ParsedCPV().setCode(item.getTextContent()).setIsMain(String.valueOf(cpvs.isEmpty())));
            }
        }
        if (cpvs.isEmpty()) {
            return null;
        }
        return cpvs;
    }


    /**
     * Parses address of implementation.
     *
     * @param entry element to parse
     * @return parsed address of implementation
     */
    private ParsedAddress parseAddressOfImplementation(final Element entry) {
        Element addressElem = (Element) getFirstNodeForName(entry, "cac:RealizedLocation");
        String country = getFirstTextForName(addressElem, "cbc:IdentificationCode");
        if (country == null || country.isEmpty()) {
            country = getFirstTextForName(addressElem, "cbc:Name");
        }
        return new ParsedAddress()
                .setCity(getFirstTextForName(addressElem, "cbc:CountrySubentity"))
                .setNuts(Arrays.asList(getFirstTextForName(addressElem, "cbc:CountrySubentityCode")))
                .setCountry(country);
    }

    /**
     * Parses lots.
     *
     * @param entry element to parse from
     * @param tenderStatus tender status
     * @return parsed lots
     */
    private List<ParsedTenderLot> parseLots(final Element entry, final String tenderStatus) {
        NodeList lotElements = entry.getElementsByTagName("cac:TenderResult");
        if (lotElements.getLength() == 0) {
            return null;
        }
        List<ParsedTenderLot> lots = new ArrayList<>();
        for (int i = 0; i < lotElements.getLength(); i++) {
            Element lotElem = (Element) lotElements.item(i);
            ParsedTenderLot lot = new ParsedTenderLot();

            Element pricesNode = (Element) getFirstNodeForName(entry, "cac:LegalMonetaryTotal");
            String currency = null, netAmount = null, amountWithWat = null;
            if (pricesNode != null) {
                Element netAmountNode = (Element) getFirstNodeForName(pricesNode, "cbc:TaxExclusiveAmount");
                if (netAmountNode != null) {
                    netAmount = netAmountNode.getTextContent();
                    currency = netAmountNode.getAttribute("currencyID");
                }
                Element amountWithWatNode = (Element) getFirstNodeForName(pricesNode, "cbc:PayableAmount");
                if (amountWithWatNode != null) {
                    amountWithWat = amountWithWatNode.getTextContent();
                    if (currency == null) {
                        currency = amountWithWatNode.getAttribute("currencyID");
                    }
                }
            }

            String status = getFirstTextForName(lotElem, "cbc:ResultCode");
            if (status == null || status.isEmpty() || tenderStatus == null || tenderStatus.isEmpty()) {
                status = null;
            } else {
                status = tenderStatus + "-" + status;
            }

            lot.setBidsCount(getFirstTextForName(lotElem, "cbc:ReceivedTenderQuantity"))
                    .setLotNumber(getFirstTextForName(lotElem, "cbc:ProcurementProjectLotID"))
                    .setStatus(status)
                    .addBid(new ParsedBid()
                            .setIsWinning("true")
                            .addBidder(parseBidder(lotElem))
                            .setPrice(new ParsedPrice()
                                    .setCurrency(currency)
                                    .setNetAmount(netAmount)
                                    .setAmountWithVat(amountWithWat)));
            if (lot.getLotNumber() == null) {
                lot.setLotNumber(String.valueOf(lots.size() + 1));
            }
            lots.add(lot);
        }
        return lots;
    }

    /**
     * Parses bidder.
     * @param lotElem element to parse
     * @return parsed bidder
     */
    private ParsedBody parseBidder(final Element lotElem) {
        if(lotElem == null) {
            return null;
        }
        Element bidderElem = (Element) getFirstNodeForName(lotElem, "cac:WinningParty");
        if(bidderElem == null) {
            return null;
        }
        List<BodyIdentifier> bodyIds = parseBodyIds((Element) getFirstNodeForName(bidderElem, "cbc:ID"));
        String name = getFirstTextForName(bidderElem, "cbc:Name");
        if(bodyIds != null || (name != null && !name.isEmpty())) {
            return new ParsedBody()
                    .setBodyIds(bodyIds)
                    .setName(name);
        }
        return null;
    }

    /**
     * Parses bid deadline.
     *
     * @param entry element to parse from.
     * @return parsed bid deadline
     */
    private String parseBidDeadline(final Element entry) {
        Element bidDeadlineElem = (Element) getFirstNodeForName(entry, "cac:TenderSubmissionDeadlinePeriod");
        if (bidDeadlineElem == null) {
            return null;
        }
        String bidDeadline = getFirstTextForName(bidDeadlineElem, "cbc:EndDate");
        if (bidDeadline == null || bidDeadline.isEmpty()) {
            return null;
        }
        String time = getFirstTextForName(bidDeadlineElem, "cbc:EndTime");
        return (time != null && !time.isEmpty()) ? bidDeadline + " " + time : bidDeadline;
    }

    /**
     * Parses publications.
     *
     * @param entry element to parse from
     * @param sourceUrl source url
     * @return parsed publications
     */
    private List<ParsedPublication> parsePublications(final Element entry, final String sourceUrl) {
        NodeList publicationNodes = entry.getElementsByTagName("cac-place-ext:ValidNoticeInfo");
        if (publicationNodes.getLength() == 0) {
            return null;
        }
        List<ParsedPublication> publications = new ArrayList<>();
        for (int i = 0; i < publicationNodes.getLength(); i++) {
            Element publElem = (Element) publicationNodes.item(i);
            ParsedPublication publication = new ParsedPublication()
                    .setPublicationDate(getFirstTextForName(publElem, "cbc:IssueDate"))
                    .setSourceFormType(getFirstTextForName(publElem, "cbc-place-ext:NoticeTypeCode"))
                    .setSource("https://www.hacienda.gob.es")
                    .setIsIncluded(false);
            publications.add(publication);
        }

        String publDate = getFirstTextForName(entry, "updated");
        if(publDate != null) {
            publDate = publDate.split("T")[0];
        }
        publications.add(new ParsedPublication()
                .setSource("https://www.hacienda.gob.es")
                .setPublicationDate(publDate)
                .setSourceTenderId(getFirstTextForName(entry, "id"))
                .setSourceFormType(PublicationFormType.COMPILED.name())
                .setIsIncluded(true)
                .setMachineReadableUrl(sourceUrl));
        return publications;
    }


    /**
     * Parses body id.
     * @param idNode node to be parsed
     * @return parsed body id
     */
    private List<BodyIdentifier> parseBodyIds(final Element idNode) {
        if (idNode == null) {
            return null;
        }
        String typeCode = idNode.getAttribute("schemeName");
        BodyIdentifier.Type type = null;
        if (typeCode != null && !typeCode.isEmpty()) {
            if (typeCode.equals("NIF")) {
                type = BodyIdentifier.Type.TAX_ID;
            } else {
                type = BodyIdentifier.Type.ORGANIZATION_ID;
            }
        }

        String id = idNode.getTextContent();
        if ((id != null && !id.isEmpty()) || type != null) {
            ArrayList<BodyIdentifier> bodyIds = new ArrayList<>();
            bodyIds.add(new BodyIdentifier().setId(id).setType(type).setScope(BodyIdentifier.Scope.ES));
            return bodyIds;
        }
        return null;
    }

    /**
     * Parses buyers.
     * @param entry element to parse
     * @return list of parsed buyers
     */
    private List<ParsedBody> parseBuyers(final Element entry) {
        NodeList buyerNodes = entry.getElementsByTagName("cac:Party");
        if (buyerNodes.getLength() == 0) {
            return null;
        }
        List<ParsedBody> buyers = new ArrayList<>();
        for (int j = 0; j < buyerNodes.getLength(); j++) {
            Element buyerNode = (Element) buyerNodes.item(j);
            Element idNode = (Element) getFirstNodeForName(buyerNode, "cbc:ID");

            ParsedBody buyer = new ParsedBody()
                    .setBodyIds(parseBodyIds(idNode))
                    .setName(getFirstTextForName(buyerNode, "cbc:Name"))
                    .setBuyerType(getFirstTextForName(entry, "cbc:ContractingPartyTypeCode"));
            String country = getFirstTextForName((Element) getFirstNodeForName(buyerNode, "cac:Country"),
                    "cbc:IdentificationCode");
            if (country == null) {
                country = getFirstTextForName((Element) getFirstNodeForName(buyerNode, "cac:Country"),
                        "cbc:Name");
            }
            ParsedAddress address = new ParsedAddress()
                    .setUrl(getFirstTextForName(buyerNode, "cbc:WebsiteURI"))
                    .setCity(getFirstTextForName(buyerNode, "cbc:CityName"))
                    .setPostcode(getFirstTextForName(buyerNode, "cbc:PostalZone"))
                    .setStreet(getFirstTextForName(buyerNode, "cbc:Line"))
                    .setCountry(country);
            if (address.getUrl() == null && address.getCity() == null && address.getPostcode() == null
                    && address.getStreet() == null && address.getCountry() == null) {
                address = null;
            }

            buyer.setBuyerType(getFirstTextForName(/*because it's out of party node*/ entry,
                    "cbc:ContractingPartyTypeCode"))
                    .setContactPoint(getFirstTextForName((Element) getFirstNodeForName(buyerNode, "cac:Contact"),
                            "cbc:Name"))
                    .setEmail(getFirstTextForName((Element) getFirstNodeForName(buyerNode, "cac:Contact"),
                            "cbc:ElectronicMail"))
                    .setAddress(address);

            buyers.add(buyer);
        }
        return buyers;
    }

    /**
     * Parses and sets estimated duration time in one of units (days, months or years).
     *
     * @param parsedTender parsed tender to set estimated duration time
     * @param entry        element to parse
     */
    private void parseAndSetDurationTime(final ParsedTender parsedTender, final Element entry) {
        Element estimatedDuration = (Element) getFirstNodeForName(entry, "cac:PlannedPeriod");
        String durationUnit = null, duration = null;
        if (estimatedDuration != null) {
            estimatedDuration = (Element) getFirstNodeForName(entry, "cbc:DurationMeasure");
            if (estimatedDuration != null) {
                duration = estimatedDuration.getTextContent();
                durationUnit = estimatedDuration.getAttribute("unitCode");
            }
        }
        if (durationUnit != null && !durationUnit.isEmpty() && duration != null && !duration.isEmpty()) {
            switch (durationUnit) {
                case "MON":
                    parsedTender.setEstimatedDurationInMonths(duration);
                    break;
                case "DAY":
                    parsedTender.setEstimatedDurationInDays(duration);
                    break;
                case "ANN":
                    parsedTender.setEstimatedDurationInYears(duration);
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * Parses final price.
     *
     * @param entry element to parse
     * @return parsed final price
     */
    private ParsedPrice parseFinalPrice(final Element entry) {
        ParsedPrice price = parsePrice(getFirstNodeForName(entry, "cbc:TaxExclusiveAmount"));
        if (price != null) {
            price.setAmountWithVat(getFirstTextForName(entry, "cbc:TotalAmount"));
        }
        return price;
    }

    /**
     * Parses deposits.
     * @param depositElem element to parse
     * @return parsed deposits
     */
    private String parseDeposits(final Element depositElem) {
        if(depositElem == null) {
            return null;
        }
        Element liabilityAmountElement = (Element) getFirstNodeForName(depositElem, "cbc:LiabilityAmount");
        if(liabilityAmountElement != null) {
            String amount = liabilityAmountElement.getTextContent();
            if(amount != null && !amount.isEmpty()) {
                String currency = liabilityAmountElement.getAttribute("currencyID");
                if(currency != null) {
                    return amount.concat(" " + currency);
                }
                return amount;
            }
        }
        Element amountRateElement = (Element) getFirstNodeForName(depositElem, "cbc:AmountRate");
        if(amountRateElement == null) {
            return null;
        }
        String ratio = amountRateElement.getTextContent();
        if(ratio != null && !ratio.isEmpty()) {
            return ratio;
        }
        return null;
    }

    /**
     * Parses selection method and award criteria and sets them to parsed tender.
     * @param parsedTender tender to set data to
     * @param selectionMethodElem element to parse data from
     */
    private void parseAndSetSelectionMethodAndAwardCriteria(final ParsedTender parsedTender,
                                                            final Element selectionMethodElem) {
        if(selectionMethodElem == null) {
            return;
        }
        NodeList criteria = selectionMethodElem.getElementsByTagName("cac:AwardingCriteria");
        if(criteria == null || criteria.getLength() == 0) {
            return;
        }

        // if only one criterion then set it to selection method, otherwise set MEAT and fill award criteria.
        if(criteria.getLength() == 1) {
            String description = getFirstTextForName((Element) criteria.item(0), "cbc:Description");
            String weight = getFirstTextForName((Element) criteria.item(0), "cbc:WeightNumeric");
            if(description != null && !description.isEmpty() && weight != null) {
                parsedTender.setSelectionMethod(description);
                ArrayList<ParsedAwardCriterion> awardCriteria = new ArrayList<>();
                awardCriteria.add(new ParsedAwardCriterion().setName(description).setWeight(weight));
                parsedTender.setAwardCriteria(awardCriteria);
            }
        } else {
            ArrayList<ParsedAwardCriterion> awardCriteria = new ArrayList<>();
            for(int i = 0; i < criteria.getLength(); i++) {
                String description = getFirstTextForName((Element) criteria.item(i), "cbc:Description");
                String weight = getFirstTextForName((Element) criteria.item(i), "cbc:WeightNumeric");
                if(description != null && !description.isEmpty() && weight != null && !weight.isEmpty()) {
                    awardCriteria.add(new ParsedAwardCriterion().setName(description).setWeight(weight));
                }
            }
            if(!awardCriteria.isEmpty()) {
                parsedTender.setAwardCriteria(awardCriteria);
                parsedTender.setSelectionMethod(SelectionMethod.MEAT.name());
            }
        }
    }

    @Override
    public List<ParsedTender> parse(final RawData raw) {
        List<ParsedTender> result = new ArrayList<>();
        Document tendersDoc = stringToXmlDocument(raw.getSourceData());
        String url = raw.getSourceUrl().toString();

        // file contains more tenders (every tender corresponds to one entry element)
        NodeList entries = tendersDoc.getElementsByTagName("entry");
        if (entries == null || entries.getLength() == 0) {
            logger.warn("No tenders in file {}", raw.getMetaData().get("fileName"));
            return result;
        }

        // parse every tender separately
        for (int i = 0; i < entries.getLength(); i++) {
            Element entry = (Element) entries.item(i);
            if (entry == null) {
                continue;
            }
            ParsedTender parsedTender = new ParsedTender();
            Element pricesNode = (Element) getFirstNodeForName(entry, "cac:BudgetAmount");
            String tenderStatus = getFirstTextForName(entry, "cbc-place-ext:ContractFolderStatusCode");

            parsedTender
                    .setTitle(getFirstTextForName(entry, "title"))
                    .setBuyerAssignedId(getFirstTextForName(entry, "cbc:ContractFolderID"))
                    .setBuyers(parseBuyers(entry))
                    .setSupplyType(getFirstTextForName(entry, "cbc:TypeCode"))
                    .setEstimatedPrice(parsePrice(getFirstNodeForName(pricesNode, "cbc:EstimatedOverallContractAmount")))
                    .setFinalPrice(parseFinalPrice(pricesNode))
                    .setCpvs(parseCpvs(entry))
                    .setAddressOfImplementation(parseAddressOfImplementation(entry))
                    .setDescription(getFirstTextForName(entry, "cbc:Description"))
                    .setAwardDecisionDate(getFirstTextForName(entry, "cbc:AwardDate"))
                    .setLots(parseLots(entry, tenderStatus))
                    .addEligibleBidLanguage(getFirstTextForName((Element) getFirstNodeForName(entry, "cac:TenderingTerms"),
                            "cbc:ID"))
                    .setNationalProcedureType(getFirstTextForName(entry, "cbc:ProcedureCode"))
                    .setIsAcceleratedProcedure(getFirstTextForName(entry, "cbc:UrgencyCode"))
                    .setBidDeadline(parseBidDeadline(entry))
                    .setPublications(parsePublications(entry, url))
                    .setDeposits(parseDeposits((Element) getFirstNodeForName(entry, "cac:RequiredFinancialGuarantee")));

            parseAndSetSelectionMethodAndAwardCriteria(parsedTender,
                    (Element) getFirstNodeForName(entry, "cac:AwardingTerms"));

            // modifies parsedTender object
            parseAndSetDurationTime(parsedTender, entry);
            result.add(parsedTender);
        }
        return result;
    }

    @Override
    protected List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "ES";
    }
}
