package eu.datlab.worker.it.parsed;

import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPayment;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Parses IT xls raw data.
 */
public class ANACTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    /**
     * Creates xml document from string.
     *
     * @param data xml data in string
     * @return xml document
     */
    private Document stringToXmlDocument(final String data) {
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
        } catch (ParserConfigurationException | SAXException | IOException e) {
            e.printStackTrace();
        }
        return doc;
    }

    /**
     * Converts xml node to string.
     *
     * @param node xml node
     * @return xml in string
     */
    private String nodeToString(final Node node) {
        StringWriter writer = new StringWriter();
        Transformer trans = null;
        try {
            trans = TransformerFactory.newInstance().newTransformer();
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            trans.setOutputProperty(OutputKeys.INDENT, "yes");
            trans.transform(new DOMSource(node), new StreamResult(writer));

            return writer.toString();
        } catch (TransformerException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Returns value from node or null if given node is null.
     *
     * @param node selected node
     * @return node value or null
     */
    private static String getValueFromXmlElement(final Node node) {
        if (node == null) {
            return null;
        }
        return node.getTextContent();
    }

    /**
     * Returns value of first xml element from list.
     *
     * @param nodeList list of elements
     * @return value of first xml element or null if no elements are provided
     */
    private static String getValueOfFirstXmlElement(final NodeList nodeList) {
        if (nodeList == null || nodeList.getLength() == 0) {
            return null;
        } else {
            return getValueFromXmlElement(nodeList.item(0));
        }
    }

    /**
     * Returns all children of node with given name.
     *
     * @param parent node
     * @param name   name to search
     * @return list of nodes with given name which are children of parent
     */
    private List<Node> getChildrenByName(final Node parent, final String name) {
        List<Node> result = new ArrayList<>();
        NodeList children = parent.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            if (children.item(i).getNodeName().equals(name)) {
                result.add(children.item(i));
            }
        }
        return result;
    }

    /**
     * Returns first child of node with given name.
     *
     * @param parent node
     * @param name   name to search
     * @return node with given name which is child of parent
     */
    private Node getFirstChildByName(final Node parent, final String name) {
        NodeList children = parent.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            if (children.item(i).getNodeName().equals(name)) {
                return children.item(i);
            }
        }
        return null;
    }

    /**
     * Parses isLeader field.
     *
     * @param node node to parse from
     * @return "true", "false" or null if there is no information about isLeader
     */
    private String parseIsLeader(final Node node) {
        Node roleNode = getFirstChildByName(node, "ruolo");
        if (roleNode == null) {
            return null;
        } else {
            String role = getValueFromXmlElement(roleNode);
            if (role != null && !role.isEmpty()) {
                if (role.equals("02-MANDATARIA") || role.equals("04-CAPOGRUPPO")) {
                    return "true";
                } else {
                    return "false";
                }
            }
        }
        return null;
    }

    /**
     * Parses bids for lot of tender.
     *
     * @param tenderDoc document for tender
     * @return list of parsed bids
     */
    private List<ParsedBid> parseBids(final Document tenderDoc) {
        List<ParsedBid> bids = new ArrayList<>();
        // partecipanti contains some partecipante and/or raggruppamento, which means bids.
        // partecipante is one bidder of one bid, raggruppamento is grouping of bidders of one bid
        NodeList tmp = tenderDoc.getElementsByTagName("partecipanti");
        if (tmp.getLength() > 0) {
            List<Node> partecipante = getChildrenByName(tmp.item(0), "partecipante");
            List<Node> raggruppamento = getChildrenByName(tmp.item(0), "raggruppamento");
            for (Node bidderNode : partecipante) {
                bids.add(new ParsedBid().setIsWinning("false").addBidder(new ParsedBody()
                        .addBodyId(new BodyIdentifier()
                                .setType(BodyIdentifier.Type.VAT)
                                .setId(getValueFromXmlElement(getFirstChildByName(bidderNode, "codiceFiscale"))))
                        .setName(getValueFromXmlElement(getFirstChildByName(bidderNode, "ragioneSociale")))));
            }
            for (Node groupNode : raggruppamento) {
                List<ParsedBody> bidders = getChildrenByName(groupNode, "membro")
                        .stream().map(a -> new ParsedBody()
                                .addBodyId(new BodyIdentifier()
                                        .setType(BodyIdentifier.Type.VAT)
                                        .setId(getValueFromXmlElement(getFirstChildByName(a, "codiceFiscale"))))
                                .setName(getValueFromXmlElement(getFirstChildByName(a, "ragioneSociale")))
                                .setIsLeader(parseIsLeader(a)))
                        .collect(Collectors.toList());

                bids.add(new ParsedBid().setBidders(bidders).setIsConsortium("true").setIsWinning("false"));
            }
        }
        return bids;
    }

    /**
     * Parses winners ids.
     *
     * @param tenderDoc document for tender
     * @return set of winners ids
     */
    private Set<String> parseWinnersIds(final Document tenderDoc) {
        List<Node> winners = new ArrayList<>();
        Set<String> winnersIds = new HashSet<>();
        NodeList tmp = tenderDoc.getElementsByTagName("aggiudicatario");
        if (tmp != null && tmp.getLength() != 0) {
            winners = Arrays.asList(tmp.item(0));
            winnersIds.addAll(winners.stream().map(a -> getValueFromXmlElement(getFirstChildByName(a,
                    "codiceFiscale"))).collect(Collectors.toSet()));
        }

        tmp = tenderDoc.getElementsByTagName("aggiudicatarioRaggruppamento");
        if (tmp != null && tmp.getLength() > 0) {
            winners = getChildrenByName(tmp.item(0), "membro");
            winnersIds.addAll(winners.stream().map(a -> getValueFromXmlElement(getFirstChildByName(a,
                    "codiceFiscale"))).collect(Collectors.toSet()));
        }
        return winnersIds;
    }

    @Override
    public final List<ParsedTender> parse(final RawData raw) {
        List<ParsedTender> parsedTenders = new ArrayList<>();

        Document doc = stringToXmlDocument(raw.getSourceData());
        String publicationDate = null, urlFile = null;
        NodeList tmp = doc.getElementsByTagName("dataUltimoAggiornamentoDataset");
        if (tmp.getLength() > 0) {
            publicationDate = getValueFromXmlElement(tmp.item(0));
        }
        tmp = doc.getElementsByTagName("urlFile");
        if (tmp.getLength() > 0) {
            urlFile = getValueFromXmlElement(tmp.item(0));
        }
        NodeList elements = doc.getElementsByTagName("lotto");
        for (int i = 0; i < elements.getLength(); i++) {
            String lotto = nodeToString(elements.item(i));
            if (lotto == null || lotto.isEmpty()) {
                continue;
            }
            Document tenderDoc = stringToXmlDocument(lotto);
            ParsedPublication publication =
                    new ParsedPublication()
                            .setSource("https://dati.anticorruzione.it")
                            .setIsIncluded(true)
                            .setPublicationDate(publicationDate)
                            .setMachineReadableUrl(urlFile)
                            .setSourceTenderId(getValueOfFirstXmlElement(tenderDoc.getElementsByTagName("cig")));
            ParsedBody buyer =
                    new ParsedBody()
                            .addBodyId(
                                    new BodyIdentifier()
                                            .setId(getValueOfFirstXmlElement(tenderDoc.getElementsByTagName("codiceFiscaleProp")))
                                            .setType(BodyIdentifier.Type.VAT))
                            .setName(getValueOfFirstXmlElement(tenderDoc.getElementsByTagName("denominazione")));

            List<ParsedBid> bids = parseBids(tenderDoc);

            Set<String> winnersIds = parseWinnersIds(tenderDoc);
            if (!winnersIds.isEmpty()) {
                bids.stream().filter(a -> a.getBidders().size() == winnersIds.size()).filter(a -> a.getBidders().stream()
                        .filter(b -> (b.getBodyIds() != null && !b.getBodyIds().isEmpty()))
                        .map(b -> winnersIds.contains(b.getBodyIds().get(0).getId())).reduce(Boolean::logicalAnd).orElse(false))
                        .forEach(b -> b.setIsWinning("true"));
            }

            ParsedBid winningBid =
                    bids.stream().filter(a -> Boolean.parseBoolean(a.getIsWinning())).findFirst().orElse(null);
            if (winningBid != null) {
                winningBid.setPrice(new ParsedPrice().setNetAmount(getValueOfFirstXmlElement(tenderDoc
                        .getElementsByTagName("importoAggiudicazione"))));

                winningBid.addPayment(new ParsedPayment().setPrice(new ParsedPrice().setNetAmount(getValueOfFirstXmlElement(tenderDoc
                        .getElementsByTagName("importoSommeLiquidate")))));
            }

            ParsedTenderLot lot =
                    new ParsedTenderLot()
                            .setEstimatedStartDate(getValueOfFirstXmlElement(tenderDoc.getElementsByTagName("dataInizio")))
                            .setEstimatedCompletionDate(getValueOfFirstXmlElement(tenderDoc.getElementsByTagName("dataUltimazione")))
                            .setBids(bids);
            ParsedTender tender =
                    new ParsedTender()
                            .addPublication(publication)
                            .addBuyer(buyer)
                            .setTitle(getValueOfFirstXmlElement(tenderDoc.getElementsByTagName("oggetto")))
                            .setNationalProcedureType(getValueOfFirstXmlElement(tenderDoc.getElementsByTagName("sceltaContraente")))
                            .addLot(lot);

            parsedTenders.add(tender);
        }
        return parsedTenders;
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "IT";
    }

    @Override
    protected final List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed,
                                                                    final RawData raw) {
        return parsed;
    }

}
