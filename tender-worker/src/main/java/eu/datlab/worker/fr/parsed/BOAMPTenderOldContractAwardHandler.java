package eu.datlab.worker.fr.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.StringUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Old contract award form parser for France.
 *
 * @author Marek Mikes
 */
final class BOAMPTenderOldContractAwardHandler {
    /**
     * Private constructor to make this class static.
     */
    private BOAMPTenderOldContractAwardHandler() {}

    /**
     * Parses data for old contract award forms.
     *
     * @param publicationElement
     *         element to parse data from
     * @param parsedTender
     *         tender to add data to
     */
    static void parse(final Element publicationElement, final ParsedTender parsedTender) {
        parsedTender
                .setAwardCriteria(parseTenderAwardCriteria(publicationElement))
                .setProcedureType(JsoupUtils.selectAttribute("DONNEES > PROCEDURES", "type", publicationElement))
                .setLots(parseTenderLots(publicationElement))
                .setAcceleratedProcedureJustification(
                        JsoupUtils.selectText("DONNEES > ANNEXE_1 > OFFRES_IRREGULIERES", publicationElement));

        parsedTender.getPublications().get(0)
                // the string can be "Date d'envoi du présent avis : 14 mars 2012."
                .setDispatchDate(StringUtils.removeDotsAtTheEnd(JsoupUtils.selectText(
                        "DONNEES > PROCEDURES > DATE_ENVOI", publicationElement)));
    }

    /**
     * Parse tender award criterion list from publication element.
     *
     * @param publicationElement
     *         publication element to be parsed
     *
     * @return award criterion list or Null
     */
    private static List<ParsedAwardCriterion> parseTenderAwardCriteria(final Element publicationElement) {
        String mainAwardCriteriaString = JsoupUtils.selectText("DONNEES > CRITERES_D_ATTRIBUTION > CRITERES_LISTE",
                publicationElement);
        if (mainAwardCriteriaString != null && !mainAwardCriteriaString.isEmpty()) {
            assert false : "We do not know this situation -> refactor parser to parse main award criterion.";
        }

        String awardCriteriaString = JsoupUtils.selectText("DONNEES > CRITERES_D_ATTRIBUTION > AUTRES",
                publicationElement);
        if (awardCriteriaString == null) {
            return null;
        }

        List<ParsedAwardCriterion> awardCriteria = new ArrayList<>();

        List<String> awardCriterionStrings = new ArrayList<>(Arrays.asList(awardCriteriaString.split(" - ")));

        for (String awardCriterionString : awardCriterionStrings) {
            // award criterion can be
            // - "- valeur technique de l'offre au vu du m�moire technique. cette valeur technique
            //   sera not�e sur 20 avec la r�partition suivante : - m�thodologie de travail et moyens mis en oeuvre
            //   (not�e sur 3) - qualit� de l'�preuve 0 (not�e sur 17) : 70 % "
            //    - the weight is at the end separated by colon
            // - "prix des fournitures 40 %."
            //    - the weight is not at the end separated by colon
            // So we have to find the weight by regular expression:
            Pattern pattern = Pattern.compile("(\\d{1,3} %)");
            Matcher matcher = pattern.matcher(awardCriterionString);
            assert matcher.groupCount() == 1;
            if (matcher.find()) {
                awardCriteria.add(new ParsedAwardCriterion()
                        .setName(awardCriterionString.substring(0, matcher.start(1)))
                        .setWeight(BOAMPTenderParserUtils.getNumberAtTheBeginning(matcher.group(1))));
            }
        }

        return awardCriteria;
    }

    /**
     * Parses all the lots from contract notice.
     *
     * @param publicationElement
     *         publication element to be parsed
     *
     * @return list of all parsed lots or empty list if no lots specified
     */
    private static List<ParsedTenderLot> parseTenderLots(final Element publicationElement) {
        Elements lotsElements = JsoupUtils.select("DONNEES > PROCEDURES > LOTS", publicationElement);
        if (lotsElements.isEmpty()) {
            return null;
        }
        assert lotsElements.size() == 1;
        Element lotsElement = lotsElements.get(0);

        List<ParsedTenderLot> lots = new ArrayList<>();

        final String bidsCountOfAllLots = JsoupUtils.selectText("DONNEES > PROCEDURES > NB_OFFRES", publicationElement);

        // - information about all lots are on the same level (child of "lots" element)
        // - sometimes lots are not separated by "NUM_LOT" element. For this situation we close actual lot and creates
        //   new lot when find information which is already set (see
        //   ftp://echanges.dila.gouv.fr/BOAMP/ANCIEN_STOCK/BOAMP/2011/MPC20110179.taz , "NOJO" = 11-209645)

        ParsedTenderLot lot = null;
        for (Element lotChildElement : lotsElement.children()) {
            if (lotChildElement.nodeName().equalsIgnoreCase("NUM_LOT")) {
                if (lotChildElement.hasText()) {
                    lot = createNewLot(lot, lots, bidsCountOfAllLots);
                    lot
                            .setLotId(lotChildElement.text());
                } else {
                    assert lot == null : "Empty lot number should be only at the beginning where lot is not created";
                    // <LOTS>
                    //   <NUM_LOT/>
                    //   <DESC>
                    //   ...
                    // or
                    // <LOTS>
                    //   <NUM_LOT/>
                    // </LOTS>
                }
            } else if (lotChildElement.nodeName().equalsIgnoreCase("DESC")) {
                lot = createLotIfDoesNotExist(lot);
                if (lot.getDescription() != null) {
                    lot = createNewLot(lot, lots, bidsCountOfAllLots);
                }
                lot.setDescription(lotChildElement.text());

            } else if (lotChildElement.nodeName().equalsIgnoreCase("NOM")) {
                lot = createLotIfDoesNotExist(lot);
                if (lot.getBids() == null) {
                    lot.addBid(new ParsedBid()
                            .setIsWinning(Boolean.TRUE.toString()));
                }
                if (lot.getBids().get(0).getBidders() == null) {
                    lot.getBids().get(0).addBidder(new ParsedBody());
                }
                if (lot.getBids().get(0).getBidders().get(0).getName() != null) {
                    lot = createNewLot(lot, lots, bidsCountOfAllLots);
                    lot
                            .addBid(new ParsedBid()
                                    .setIsWinning(Boolean.TRUE.toString())
                                    .addBidder(new ParsedBody()));
                }
                lot.getBids().get(0).getBidders().get(0).setName(lotChildElement.text());
            } else if (lotChildElement.nodeName().equalsIgnoreCase("TEL")) {
                lot = createLotIfDoesNotExist(lot);
                if (lot.getBids() == null) {
                    lot.addBid(new ParsedBid()
                            .setIsWinning(Boolean.TRUE.toString()));
                }
                if (lot.getBids().get(0).getBidders() == null) {
                    lot.getBids().get(0).addBidder(new ParsedBody());
                }
                if (lot.getBids().get(0).getBidders().get(0).getPhone() != null) {
                    lot = createNewLot(lot, lots, bidsCountOfAllLots);
                    lot
                            .addBid(new ParsedBid()
                                    .setIsWinning(Boolean.TRUE.toString())
                                    .addBidder(new ParsedBody()));
                }
                lot.getBids().get(0).getBidders().get(0)
                        .setPhone(lotChildElement.text());
            } else if (lotChildElement.nodeName().equalsIgnoreCase("ADRESSE")) {
                lot = createBidderAddressIfDoesNotExist(lot);
                if (lot.getBids().get(0).getBidders().get(0).getAddress().getStreet() != null) {
                    lot = createNewLot(lot, lots, bidsCountOfAllLots);
                    lot
                            .addBid(new ParsedBid()
                                    .setIsWinning(Boolean.TRUE.toString())
                                    .addBidder(new ParsedBody()
                                            .setAddress(new ParsedAddress())));
                }
                lot.getBids().get(0).getBidders().get(0).getAddress()
                        .setStreet(lotChildElement.text());
            } else if (lotChildElement.nodeName().equalsIgnoreCase("CP")) {
                lot = createBidderAddressIfDoesNotExist(lot);
                if (lot.getBids().get(0).getBidders().get(0).getAddress().getPostcode() != null) {
                    lot = createNewLot(lot, lots, bidsCountOfAllLots);
                    lot
                            .addBid(new ParsedBid()
                                    .setIsWinning(Boolean.TRUE.toString())
                                    .addBidder(new ParsedBody()
                                            .setAddress(new ParsedAddress())));
                }
                lot.getBids().get(0).getBidders().get(0).getAddress()
                        .setPostcode(lotChildElement.text());
            } else if (lotChildElement.nodeName().equalsIgnoreCase("VILLE")) {
                lot = createBidderAddressIfDoesNotExist(lot);
                if (lot.getBids().get(0).getBidders().get(0).getAddress().getCity() != null) {
                    lot = createNewLot(lot, lots, bidsCountOfAllLots);
                    lot
                            .addBid(new ParsedBid()
                                    .setIsWinning(Boolean.TRUE.toString())
                                    .addBidder(new ParsedBody()
                                            .setAddress(new ParsedAddress())));
                }
                lot.getBids().get(0).getBidders().get(0).getAddress()
                        .setCity(lotChildElement.text());
            } else if (lotChildElement.nodeName().equalsIgnoreCase("MONTANT_HT")
                || lotChildElement.nodeName().equalsIgnoreCase("MONTANT")) {
                parseLotFinalPrice(lotChildElement, lot, lots, bidsCountOfAllLots);
            } else if (lotChildElement.nodeName().equalsIgnoreCase("DATE_ATT_MARCHE")) {
                lot = createLotIfDoesNotExist(lot);
                if (lot.getAwardDecisionDate() != null) {
                    lot = createNewLot(lot, lots, bidsCountOfAllLots);
                }
                lot.setAwardDecisionDate(lotChildElement.text());
            } else if (lotChildElement.nodeName().equalsIgnoreCase("NB_OFFRES")) {
                lot = createLotIfDoesNotExist(lot);
                if (lot.getBidsCount() != null) {
                    lot = createNewLot(lot, lots, bidsCountOfAllLots);
                }
                lot.setBidsCount(lotChildElement.text());
            }
        }

        // add last parsed lot
        if (lot != null) {
            lots.add(lot);
        }

        return lots;
    }

    /**
     * Creates lot if it does not exist. It is convenience method for parsing lots - otherwise the method would be
     * too long.
     *
     * @param lot
     *         tender lot
     *
     * @return not null lot
     */
    private static ParsedTenderLot createLotIfDoesNotExist(final ParsedTenderLot lot) {
        return lot == null ? new ParsedTenderLot() : lot;
    }

    /**
     * Creates address of bidder if it does not exist. It is convenience method for parsing lots - otherwise the method
     * would be too long.
     *
     * @param lot
     *         tender lot
     *
     * @return not null lot
     */
    private static ParsedTenderLot createBidderAddressIfDoesNotExist(final ParsedTenderLot lot) {
        final ParsedTenderLot resultingLot = createLotIfDoesNotExist(lot);
        if (resultingLot.getBids() == null) {
            resultingLot.addBid(new ParsedBid()
                    .setIsWinning(Boolean.TRUE.toString()));
        }
        if (resultingLot.getBids().get(0).getBidders() == null) {
            resultingLot.getBids().get(0).addBidder(new ParsedBody());
        }
        if (resultingLot.getBids().get(0).getBidders().get(0).getAddress() == null) {
            resultingLot.getBids().get(0).getBidders().get(0).setAddress(new ParsedAddress());
        }
        return resultingLot;
    }

    /**
     * Parses final price of lot (it means price of winner bidder). It is convenience method for parsing lots -
     * otherwise the method would be too long.
     *
     * @param lotChildElement
     *         child element lot element, which is parent element for the all lots
     * @param lot
     *         tender lot
     * @param lots
     *         parsed tender lots
     * @param bidsCountOfAllLots
     *         bids count parsed outside the <lots> element
     *
     * @return not null actual lot
     */
    private static ParsedTenderLot parseLotFinalPrice(final Element lotChildElement,
                                                      final ParsedTenderLot lot,
                                                      final List<ParsedTenderLot> lots,
                                                      final String bidsCountOfAllLots) {
        ParsedTenderLot resultedLot = lot;
        final String priceString = lotChildElement.text();
        // price can look like:
        //   <MONTANT_HT>22 289,86 (H.T./an).</MONTANT_HT>
        // or
        //   <MONTANT_HT>1 458 029,36 EUR.</MONTANT_HT>
        //   <MONTANT_HT> EUR.</MONTANT_HT>
        //   <MONTANT_HT>12 433,24 EUR EUR.</MONTANT_HT>
        //   <MONTANT_HT>84 542,42 EURht EUR.</MONTANT_HT>
        //   <MONTANT_HT>212 816,74 euros EUR.</MONTANT_HT>
        //   <MONTANT_HT>1 572 6,8 3ht EUR.</MONTANT_HT>
        if (priceString.contains("(")) {
            resultedLot = createLotIfDoesNotExist(resultedLot);
            if (resultedLot.getBids() == null) {
                resultedLot.addBid(new ParsedBid()
                        .setIsWinning(Boolean.TRUE.toString()));
            }
            if (resultedLot.getBids().get(0).getPrice() != null) {
                resultedLot = createNewLot(resultedLot, lots, bidsCountOfAllLots);
                resultedLot
                        .addBid(new ParsedBid()
                                .setIsWinning(Boolean.TRUE.toString()));
            }
            resultedLot.getBids().get(0)
                    .setPrice(new ParsedPrice()
                            .setNetAmount(priceString.substring(0, priceString.indexOf('('))));
        } else {
            Pattern pattern = Pattern.compile("([a-zA-Z].*)");
            Matcher matcher = pattern.matcher(priceString);
            if (matcher.find()) {
                resultedLot = createLotIfDoesNotExist(resultedLot);
                if (resultedLot.getBids() == null) {
                    resultedLot.addBid(new ParsedBid()
                            .setIsWinning(Boolean.TRUE.toString()));
                }
                if (resultedLot.getBids().get(0).getPrice() != null) {
                    resultedLot = createNewLot(resultedLot, lots, bidsCountOfAllLots);
                    resultedLot
                            .addBid(new ParsedBid()
                                    .setIsWinning(Boolean.TRUE.toString()));
                }
                final String currency = matcher.group(1);
                resultedLot.getBids().get(0)
                        .setPrice(new ParsedPrice()
                                .setNetAmount(priceString.substring(0, matcher.start(1)))
                                .setCurrency(currency.contains("EUR")
                                        ? "EUR" : StringUtils.removeDotsAtTheEnd(currency)));
            }
        }
        return resultedLot;
    }

    /**
     * Adds lot into list of lots when the lot is not null. And creates new lot.
     * Moreover, when the lot does not have the bids count filled, the general bids count is filled.
     *
     * @param oldLot
     *         old tender lot
     * @param lots
     *         parsed tender lots
     * @param bidsCountOfAllLots
     *         bids count parsed outside the <lots> element
     *
     * @return not null actual lot
     */
    private static ParsedTenderLot createNewLot(final ParsedTenderLot oldLot,
                                                final List<ParsedTenderLot> lots,
                                                final String bidsCountOfAllLots) {
        if (oldLot != null) {
            if (oldLot.getBidsCount() == null) {
                oldLot.setBidsCount(bidsCountOfAllLots);
            }
            lots.add(oldLot);
        }
        return new ParsedTenderLot();
    }

}
