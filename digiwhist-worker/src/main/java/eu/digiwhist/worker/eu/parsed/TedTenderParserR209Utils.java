package eu.digiwhist.worker.eu.parsed;

import static eu.digiwhist.worker.eu.parsed.TedTenderParserUtils.getDefaultNuts;
import static eu.digiwhist.worker.eu.parsed.TedTenderParserUtils.parseBodyIdentifier;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * Class provides set of functions for parsing typical xml fragments like address, subject etc.
 *
 * @author Tomas Mrazek
 */
final class TedTenderParserR209Utils {

    /**
     * Source for TED publications.
     */
    private static final String PUBLICATIONS_SOURCE = PublicationSources.EU_TED;

    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedTenderParserR209Utils() {
        throw new AssertionError();
    }

    /**
     * Returns section I node (Contracting authority) of the TED form.
     *
     * @param originNode
     *      node of origin point for parsing
     * @return section node
     */
    public static Element getSectionI(final Element originNode) {
        return JsoupUtils.selectFirst("CONTRACTING_BODY", originNode);
    }

    /**
     * Returns section II node (Object) of the TED form.
     *
     * @param originNode
     *      node of origin point for parsing
     * @return section node
     */
    public static Element getSectionII(final Element originNode) {
        return JsoupUtils.selectFirst("OBJECT_CONTRACT", originNode);
    }

    /**
     * Returns section III node (Legal, economic, financial and technical information) of the TED form.
     *
     * @param originNode
     *      node of origin point for parsing
     * @return section node
     */
    public static Element getSectionIII(final Element originNode) {
        return JsoupUtils.selectFirst("LEFTI", originNode);
    }

    /**
     * Returns section IV node (Procedure) of the TED form.
     *
     * @param originNode
     *      node of origin point for parsing
     * @return section node
     */
    public static Element getSectionIV(final Element originNode) {
        return JsoupUtils.selectFirst("PROCEDURE", originNode);
    }

    /**
     * Returns section V node (Award of contract) of the TED form.
     *
     * @param originNode
     *      node of origin point for parsing
     * @return section node
     */
    public static Element getSectionV(final Element originNode) {
        return JsoupUtils.selectFirst("COMPLEMENTARY_INFO", originNode);
    }

    /**
     * Returns section VI node (Procedure) of the TED form.
     *
     * @param originNode
     *      node of origin point for parsing
     * @return section node
     */
    public static Element getSectionVI(final Element originNode) {
        return JsoupUtils.selectFirst("COMPLEMENTARY_INFO", originNode);
    }

    /**
     * Parses main tender publication.
     *
     * @param document
     *         parsed document
     * @return parsed main publication
     */
    public static ParsedPublication parseMainPublication(final Document document) {
        Element originNode = TedTenderParserUtils.getOriginNode(document);
        return TedTenderParserUtils.initMainPublication(document)
            .setDispatchDate(JsoupUtils.selectText("COMPLEMENTARY_INFO > DATE_DISPATCH_NOTICE", originNode))
            .setSourceTenderId(JsoupUtils.selectText("OBJECT_CONTRACT > REFERENCE_NUMBER", originNode))
            .setSource(PUBLICATIONS_SOURCE);
    }

    /**
     * Parses previous publication concerning this procedure.
     *
     * @param originNode
     *      origin node for document parsing
     * @return previous publication
     */
    public static ParsedPublication parsePreviousPublication(final Element originNode) {
        if (!JsoupUtils.exists("PROCEDURE > NOTICE_NUMBER_OJ", originNode)) {
            return null;
        }

        return new ParsedPublication()
            .setBuyerAssignedId(JsoupUtils.selectText("PROCEDURE > NOTICE_NUMBER_OJ", originNode))
            .setIsIncluded(false)
            .setSource(PUBLICATIONS_SOURCE);
    }

    /**
     * Parses eligible languages.
     *
     * @param languageNodes
     *      language nodes
     * @return list of languages
     */
    public static List<String> parseEligibleBidLanguages(final Elements languageNodes) {
        if (languageNodes == null || languageNodes.isEmpty()) {
            return null;
        }

        final List<String> languages = new ArrayList();
        for (final Element node : languageNodes) {
            languages.add(JsoupUtils.selectAttribute("VALUE", node));
        }

        return languages;
    }

    /**
     * Prases tender procedure type.
     *
     * @param procedureNode
     *      node with procedure data
     * @return procedure type
     */
    public static String parseProcedureType(final Element procedureNode) {
        final Element procedureTypeNode = JsoupUtils.selectFirst(
            "PT_OPEN, PT_RESTRICTED, PT_COMPETITIVE_NEGOTIATION, PT_COMPETITIVE_DIALOGUE, PT_INNOVATION_PARTNERSHIP,"
                + "PT_NEGOTIATED_WITH_PRIOR_CALL, PT_NEGOTIATED_WITH_COMPETITION",
            procedureNode);

        return (procedureTypeNode == null ? null : procedureTypeNode.tagName());
    }

    /**
     * Parses price from the given node.
     * @param priceNode
     *      node with price data
     * @return parsed price
     */
    public static ParsedPrice parsePrice(final Element priceNode) {
        if (priceNode == null) {
            return null;
        }

        return new ParsedPrice()
            .setNetAmount(priceNode.text())
            .setCurrency(JsoupUtils.selectAttribute("CURRENCY", priceNode));
    }

    /**
     * Parses prices range from the given node.
     * @param priceNode
     *      node with price range data
     * @return parsed price
     */
    public static ParsedPrice parsePriceRange(final Element priceNode) {
        if (priceNode == null) {
            return null;
        }

        return new ParsedPrice()
            .setCurrency(JsoupUtils.selectAttribute("CURRENCY", priceNode))
            .setMinNetAmount(JsoupUtils.selectText("LOW", priceNode))
            .setMaxNetAmount(JsoupUtils.selectText("HIGH", priceNode));
    }

    /**
     * Prases price and sets max and min amount if specified.
     *
     * @param contractNode
     *      node with contract data
     * @return parsed price
     */
    public static ParsedPrice parsePriceWithRange(final Element contractNode) {
        ParsedPrice price = parsePrice(JsoupUtils.selectFirst("VAL_TOTAL", contractNode));
        final Element priceRangeNode = JsoupUtils.selectFirst("VAL_RANGE_TOTAL", contractNode);

        if (priceRangeNode != null) {
            if (price == null) {
                price = new ParsedPrice()
                    .setCurrency(JsoupUtils.selectAttribute("CURRENCY", priceRangeNode));
            }

            final ParsedPrice priceRange = parsePriceRange(priceRangeNode);
            if (priceRange != null) {
                price
                    .setMaxNetAmount(priceRange.getMaxNetAmount())
                    .setMinNetAmount(priceRange.getMinNetAmount());
            }
        }

        return price;
    }

    /**
     * Parses body from node.
     *
     * @param bodyNode
     *         node with body data
     *
     * @return parsed body
     */
    public static ParsedBody parseBody(final Element bodyNode) {
        if (bodyNode == null) {
            return null;
        }

        final ParsedBody body = new ParsedBody()
            .setName(JsoupUtils.selectText("OFFICIALNAME", bodyNode))
            //address has same structure as form prior R2.0.9
            .setAddress(TedTenderParserUtils.parseAddress(bodyNode))
            .setEmail(JsoupUtils.selectText("E_MAIL", bodyNode))
            .setContactName(JsoupUtils.selectText("CONTACT_POINT", bodyNode))
            .setPhone(JsoupUtils.selectText("PHONE", bodyNode));

        body.addBodyId(parseBodyIdentifier(JsoupUtils.selectFirst("NATIONALID", bodyNode), body.getAddress()));

        return body;
    }

    /**
     * Parses bidder.
     *
     * @param bidderNode
     *      node with bidder data
     * @return parsed bidder
     */
    public static ParsedBody parseBidder(final Element bidderNode) {
        return TedTenderParserR209Utils.parseBody(bidderNode)
            .setIsSme(JsoupUtils.exists("SME", bidderNode).toString());
    }

    /**
     * Parses bid.
     *
     * @param bidNode
     *      node with bid data
     * @return parsed bid
     */
    public static ParsedBid parseBid(final Element bidNode) {
        return new ParsedBid()
            .addBidder(TedTenderParserR209Utils.parseBidder(JsoupUtils.selectFirst("CONTRACTOR", bidNode)))
            .setIsSubcontracted(JsoupUtils.exists("LIKELY_SUBCONTRACTED", bidNode).toString())
            .setSubcontractedValue(TedTenderParserR209Utils.parsePrice(
                JsoupUtils.selectFirst("VAL_SUBCONTRACTING", bidNode)))
            .setSubcontractedProportion(JsoupUtils.selectText("PCT_SUBCONTRACTING", bidNode))
            .setPrice(TedTenderParserR209Utils.parsePriceWithRange(bidNode))
            .setIsConsortium(JsoupUtils.exists("AWARDED_TO_GROUP", bidNode).toString());
    }

    /**
     * Parses all CPVs from the given node.
     *
     * @param cpvsNode
     *      node with CPVs data
     * @return list of CPVs
     */
    public static List<ParsedCPV> parseCpvs(final Element cpvsNode) {
        final Elements cpvNodes = JsoupUtils.select("CPV_CODE, CPV_SUPPLEMENTARY_CODE", cpvsNode);
        if (cpvNodes == null || cpvNodes.isEmpty()) {
            return null;
        }

        final List<ParsedCPV> cpvs = new ArrayList();
        for (final Element node : cpvNodes) {
            cpvs.add(new ParsedCPV()
                .setCode(JsoupUtils.selectAttribute("CODE", node))
                .setIsMain(Boolean.toString(node.tagName().equalsIgnoreCase("CPV_CODE")))
            );
        }

        return cpvs;
    }

    /**
     * Parses address of implementation.
     *
     * @param lotNode
     *      node with lot data
     * @return address of implementation
     */
    public static ParsedAddress parseAddressOfImplementation(final Element lotNode) {
        String nuts = JsoupUtils.selectAttribute("NUTS", "CODE", lotNode);
        if (nuts == null) {
            nuts = getDefaultNuts(lotNode.ownerDocument());
        }

        return new ParsedAddress()
            .addNuts(nuts)
            .setRawAddress(JsoupUtils.selectText("MAIN_SITE", lotNode));
    }

    /**
     * Parses award criteri.
     *
     * @param lotNode
     *      node with lot data
     * @return list of award criteria
     */
    public static List<ParsedAwardCriterion> parseAwardCriteria(final Element lotNode) {
        final Elements criterionNodes = JsoupUtils.select("AC_QUALITY, AC_COST, AC_PRICE, AC_PROCUREMENT_DOC", lotNode);
        if (criterionNodes == null || criterionNodes.isEmpty()) {
            return null;
        }

        final List<ParsedAwardCriterion> criteria = new ArrayList();
        for (final Element node : criterionNodes) {
            final String criterionName = JsoupUtils.selectText("AC_CRITERION", node);
            criteria.add(new ParsedAwardCriterion()
                .setName(criterionName == null ? node.tagName() : criterionName)
                .setWeight(JsoupUtils.selectText("AC_WEIGHTING", node))
                .setIsPriceRelated(node.tagName().equals("AC_PRICE") ? Boolean.TRUE.toString() : null));
        }

        return criteria;
    }

    /**
     * Parses fundings.
     *
     * @param fundingNode
     *      node with funding data
     * @return list of fundings
     */
    public static ParsedFunding parseFunding(final Element fundingNode) {
        return new ParsedFunding()
            .setIsEuFund(JsoupUtils.exists("EU_PROGR_RELATED", fundingNode).toString())
            .setProgramme(JsoupUtils.selectText("EU_PROGR_RELATED", fundingNode));
    }

    /**
     * Parses buyer type.
     *
     * @param buyerTypeNode
     *          buyer type node
     * @return buyer type
     */
    public static String parseBuyerType(final Element buyerTypeNode) {
        if (buyerTypeNode == null) {
            return null;
        }

        return (buyerTypeNode.hasAttr("VALUE") ? buyerTypeNode.attr("VALUE") : buyerTypeNode.text());
    }

    /**
     * Parses npwp reasons.
     *
     * @param context
     *      context that includes npwp reasons nodes
     * @return list of npwp reasons
     */
    public static List<String> parseNpwpReasons(final Element context) {
        Elements npwpReasonNodes = JsoupUtils.select("*:empty", context);
        if (npwpReasonNodes == null || npwpReasonNodes.isEmpty()) {
            return null;
        }

        final List<String> reasons = new ArrayList<>();
        npwpReasonNodes.forEach(n -> {
            reasons.add(n.nodeName());
        });

        return reasons.isEmpty() ? null : reasons;
    }

    /**
     * Parses buyers.
     *
     * @param doc
     *         parsed document
     * @return list of parsed buyers
     */
    public static List<ParsedBody> parseBuyers(final Document doc) {
        final Elements bodyNodes = JsoupUtils.select("ADDRESS_CONTRACTING_BODY, ADDRESS_CONTRACTING_BODY_ADDITIONAL",
                TedTenderParserR209Utils.getSectionI(TedTenderParserUtils.getOriginNode(doc)));

        if (bodyNodes == null || bodyNodes.isEmpty()) {
            return null;
        }

        final List<ParsedBody> buyers = new ArrayList<>();
        for (Element n : bodyNodes) {
            final ParsedBody buyer = TedTenderParserR209Utils.parseBody(n);

            buyer.setIsLeader(String.valueOf(n.tagName().equalsIgnoreCase("ADDRESS_CONTRACTING_BODY")));

            buyer.setMainActivities(TedTenderParserUtils.parseBuyerMainActivities(JsoupUtils.select("CE_ACTIVITY,"
                + " CE_ACTIVITY_OTHER, CA_ACTIVITY, CA_ACTIVITY_OTHER", n.parent()), doc));

            buyer.setBuyerType(TedTenderParserUtils.getDefaultBuyerType(doc));
            if (buyer.getBuyerType() == null) {
                buyer.setBuyerType(TedTenderParserR209Utils.parseBuyerType(
                    JsoupUtils.selectFirst("CE_TYPE, CE_TYPE_OTHER, CA_TYPE, CA_TYPE_OTHER", n.parent())));
            }

            buyers.add(buyer);
        }

        return buyers;
    }

    /**
     * Parses lots from node.
     *
     * @param lotsNodes
     *         node with lots data
     * @param originNode
     *         node of origin point for parsing
     * @return list of parsed lots or null if list is empty
     */
    public static List<ParsedTenderLot> parseLots(final Elements lotsNodes, final Element originNode) {
        if (lotsNodes == null || lotsNodes.isEmpty()) {
            return null;
        }

        final List<ParsedTenderLot> lots = new ArrayList();

        int position = 1;
        for (final Element lotNode : lotsNodes) {
            final String id = JsoupUtils.selectAttribute("ITEM", lotNode);

            final ParsedTenderLot lot = parseLot(lotNode)
                .setPositionOnPage(String.valueOf(position++));

            final Element lotAwardNode = JsoupUtils.selectFirst("AWARD_CONTRACT[ITEM=" + id + "]", originNode);
            if (lotAwardNode != null) {
                lot.setContractNumber(JsoupUtils.selectText("CONTRACT_NO", lotAwardNode))
                    .setTitle(JsoupUtils.selectText("TITLE", lotAwardNode));

                final Element awardedContractNode = JsoupUtils.selectFirst("AWARDED_CONTRACT, NO_AWARDED_CONTRACT",
                        lotAwardNode);

                boolean isAwarded = false;
                if (awardedContractNode != null) {
                    isAwarded = awardedContractNode.tagName().equalsIgnoreCase("AWARDED_CONTRACT");
                    
                    Element statusNode = awardedContractNode;
                    if (!isAwarded && awardedContractNode.childNodeSize() > 0) {
                        statusNode = awardedContractNode.child(0);
                    }
                    lot.setStatus(statusNode.tagName());
                }
                lot.setIsAwarded(String.valueOf(isAwarded));
                
                if (isAwarded) {
                    lot.setBidsCount(JsoupUtils.selectText("NB_TENDERS_RECEIVED", awardedContractNode))
                        .setSmeBidsCount(JsoupUtils.selectText("NB_TENDERS_RECEIVED_SME", awardedContractNode))
                        .setOtherEuMemberStatesCompaniesBidsCount(
                                JsoupUtils.selectText("NB_TENDERS_RECEIVED_OTHER_EU", awardedContractNode))
                        .setNonEuMemberStatesCompaniesBidsCount(
                                JsoupUtils.selectText("NB_TENDERS_RECEIVED_NON_EU", awardedContractNode))
                        .setElectronicBidsCount(
                                JsoupUtils.selectText("NB_TENDERS_RECEIVED_EMEANS", awardedContractNode))
                        .addBid(TedTenderParserR209Utils.parseBid(awardedContractNode)
                                .setIsWinning(Boolean.TRUE.toString()))
                        .setEstimatedPrice(TedTenderParserR209Utils.parsePrice(
                                JsoupUtils.selectFirst("VAL_ESTIMATED_TOTAL", lotNode)))                        
                        .setAwardDecisionDate(JsoupUtils.selectText("DATE_CONCLUSION_CONTRACT", awardedContractNode));
                }
            }

            lots.add(lot);
        }

        return lots;
    }

    /**
     * Parses lot from the given lot node.
     *
     * @param lotNode
     *         node with lot data
     *
     * @return parsed lot
     */
    public static ParsedTenderLot parseLot(final Element lotNode) {
        return new ParsedTenderLot()
            .setLotNumber(JsoupUtils.selectText("LOT_NO", lotNode))
            .setTitle(JsoupUtils.selectText("TITLE", lotNode))
            .setDescription(JsoupUtils.selectText("SHORT_DESCR", lotNode))
            .setEstimatedPrice(TedTenderParserR209Utils.parsePrice(JsoupUtils.selectFirst("VAL_OBJECT", lotNode)))
            .setCpvs(TedTenderParserR209Utils.parseCpvs(JsoupUtils.selectFirst("CPV_ADDITIONAL", lotNode)))
            .setAddressOfImplementation(TedTenderParserR209Utils.parseAddressOfImplementation(lotNode))
            .setAwardCriteria(TedTenderParserR209Utils.parseAwardCriteria(lotNode))
            .addFunding(TedTenderParserR209Utils.parseFunding(lotNode))
            .setHasOptions(JsoupUtils.exists("OPTIONS", lotNode).toString());
    }
}
