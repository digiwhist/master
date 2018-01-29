package eu.digiwhist.worker.pl.parsed;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import java.util.Arrays;

/**
 * Parser for contract notice form specific data.
 *
 * @author Tomas Mrazek
 */
public final class UZPContractAwardNoticeHandler {
    /**
     * Suppress default constructor for noninstantiability.
     */
    private UZPContractAwardNoticeHandler() {
        throw new AssertionError();
    }

    /**
     * @param document
     *         xml document
     * @param machineReadableUrl
     *      machine readable URL of included publication
     * @return list of parsed tenders
     */
    public static List<ParsedTender> parse(final Document document, final String machineReadableUrl) {
        return Arrays.asList(UZPTenderParserUtils.parseCommonFormData(document, machineReadableUrl)
            .addPublications(parsePublications(document))
            .setEstimatedPrice(parseEstimatedPrice(document))
            .addFunding(parseFunding(document))
            .addNpwpReason(JsoupUtils.selectText("zamowienie_uzasadnienie, zal_uzasadnienie", document))
            .setLots(parseLots(document)));
    }

    /**
     * Parses funding.
     *
     * @param document
     *      parsed document
     * @return funding or null
     */
    private static ParsedFunding parseFunding(final Document document) {
        String project = JsoupUtils.selectText("projekt", document);
        if (project == null) {
            return null;
        }

        return new ParsedFunding()
            .setProgramme(project)
            .setIsEuFund(UZPTenderParserUtils.isEnabled("zamowienie_ue", document).toString());
    }

    /**
     * Parses estimated price.
     *
     * @param document
     *      parsed document
     * @return estimated price or null
     */
    private static ParsedPrice parseEstimatedPrice(final Document document) {
        String amount = JsoupUtils.selectText("wartosc, szacunkowa_wart_zam", document);
        if (amount == null) {
            return null;
        }

        return new ParsedPrice()
            .setNetAmount(amount)
            .setCurrency(JsoupUtils.selectText("waluta", document));
    }

    /**
     * Parses publications.
     *
     * @param document
     *      parsed document
     * @return non-empty list of publications or null
     */
    private static List<ParsedPublication> parsePublications(final Document document) {
        List<ParsedPublication> publications = new ArrayList<>();

        String sourceId = JsoupUtils.selectText("biul_pub_poz", document);
        if (sourceId != null) {
            publications.add(new ParsedPublication()
                .setIsIncluded(false)
                .setSource(PublicationSources.PL_UZP_FTP)
                .setSourceId(sourceId)
                .setPublicationDate(JsoupUtils.selectText("biul_pub_rok", document)));
        }

        sourceId = JsoupUtils.selectText("nrpozycji", document);
        if (sourceId != null) {
            publications.add(new ParsedPublication()
                .setIsIncluded(false)
                .setSource(PublicationSources.PL_UZP_FTP)
                .setSourceId(sourceId)
                .setPublicationDate(JsoupUtils.selectText("datawydaniabiuletynu", document)));
        }
        
        return publications.isEmpty() ? null : publications;
    }

    /**
     * Parses tender lots.
     *
     * @param document
     *         document
     *
     * @return list of parsed lots
     */
    private static List<ParsedTenderLot> parseLots(final Element document) {
        final Elements lotNodes = JsoupUtils.select("czesci > *", document);
        if (lotNodes == null || lotNodes.isEmpty()) {
            return null;
        }

        int positionOnPage = 0;
        final List<ParsedTenderLot> lots = new ArrayList<>();        
        for (Element node : lotNodes) {
            ParsedTenderLot lot = new ParsedTenderLot()
                .setPositionOnPage(String.valueOf(positionOnPage))
                .setTitle(JsoupUtils.selectText("nazwa", node))
                .setLotNumber(JsoupUtils.selectText("nr_czesci_1", node))
                .setAwardDecisionDate(JsoupUtils.selectText("data_zam", node))
                .setBidsCount(JsoupUtils.selectText("liczba_ofert", node))
                .setValidBidsCount(JsoupUtils.selectText("liczba_odrzuconych_ofert", node))
                .setBids(parseBids(node));
                
            String price = JsoupUtils.selectText("wartosc", node);
            if (price != null) {
                lot.setEstimatedPrice(new ParsedPrice()
                        .setNetAmount(price)
                        .setCurrency(JsoupUtils.selectText("waluta", document)));
            }

            String city = JsoupUtils.selectText("miejsce", node);
            if (city != null) {
                lot.setAddressOfImplementation(new ParsedAddress().setCity(city));
            }

            lots.add(lot);

            positionOnPage++;
        }

        return lots;
    }

    /**
     * Parses lot bids.
     *
     * @param lotNode
     *         lot node
     *
     * @return list of lot bids
     */
    private static List<ParsedBid> parseBids(final Element lotNode) {
        final Elements biddersNodes = JsoupUtils.select("wykonawcy > *", lotNode);
        final List<ParsedBid> bids = new ArrayList<>();

        if (biddersNodes.isEmpty()) {
            bids.add(parseBid(lotNode, lotNode));
        } else {
            for (Element bidderNode : biddersNodes) {
                bids.add(parseBid(bidderNode, lotNode));
            }
        }

        return (bids.isEmpty() ? null : bids);
    }

    /**
     * Parses bid.
     *
     * @param bidderNode
     *         bid node
     * @param bidNode
     *         bid node
     *
     * @return parsed bid
     */
    private static ParsedBid parseBid(final Element bidderNode, final Element bidNode) {
        return new ParsedBid()
            .setIsWinning(Boolean.TRUE.toString())
            .addBidder(new ParsedBody()
                .setName(JsoupUtils.selectText("nazwa_wyk, nazwa", bidderNode))
                .setAddress(praseBidderAddress(bidderNode)))
            .setPrice(parseBidPrice(bidNode));
    }

    /**
     * Parses bidder address.
     *
     * @param lotNode
     *         lot node
     *
     * @return bidder address
     */
    private static ParsedAddress praseBidderAddress(final Element lotNode) {
        return new ParsedAddress()
            .setStreet(JsoupUtils.selectText("adres", lotNode))
            .setCity(JsoupUtils.selectText("miejsc", lotNode))
            .setPostcode(JsoupUtils.selectText("kod", lotNode))
            .setState(JsoupUtils.selectText("wojewodztwo", lotNode));
    }

    /**
     * Prases price of bid.
     *
     * @param lotNode
     *         lot node
     *
     * @return bid price
     */
    private static ParsedPrice parseBidPrice(final Element lotNode) {
        return new ParsedPrice()
            .setNetAmount(JsoupUtils.selectText("cena", lotNode))
            .setMinNetAmount(JsoupUtils.selectText("cena_min", lotNode))
            .setMaxNetAmount(JsoupUtils.selectText("cena_max", lotNode))
            .setCurrency(JsoupUtils.selectText("waluta", lotNode));
    }
}
