package eu.datlab.worker.nl.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_II_1_1_TITLE_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_II_1_6_TITLE_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_II_1_7_TITLE_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_II_1_8_TITLE_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_II_2_13_TITLE_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_II_2_14_TITLE_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_II_2_TITLE_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_I_5_TITLE_SELECTOR;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang.BooleanUtils;

/**
 * Parser for TenderNed new contract award form specific data.
 *
 * @author Marek Mikes
 */
final class TenderNedTenderNewContractAwardHandler {
    /**
     * Private constructor to make this class static.
     */
    private TenderNedTenderNewContractAwardHandler() {
    }

    /**
     * Parse method for TenderNed new contract award form from specific data.
     *
     * @param parsedTender
     *         tender to add data to
     * @param form
     *         document to parse data from
     *
     * @return ParsedTender with data added
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Element form) {
        TenderNedTenderOldAndNewFormUtils.parseCommonAttributes(parsedTender, form);

        parsedTender.getBuyers().get(0).getAddress()
            .setUrl(TenderNedTenderOldAndNewFormUtils.parseNewBuyerUrl(form));

        parsedTender.getBuyers().get(0)
            .setBuyerType(TenderNedTenderOldAndNewFormUtils.parseNewTenderBuyerType(form))
            .setMainActivities(parseBuyerMainActivity(form));

        parsedTender
            .setCpvs(TenderNedTenderOldAndNewFormUtils.parseNewTenderCpvs(form))
            .setHasLots(TenderNedTenderOldAndNewFormUtils.parseIfTenderHasLots(ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_6_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_7_TITLE_SELECTOR, form))))
            .addFunding(TenderNedTenderOldAndNewFormUtils.parseTenderFunding(ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_2_13_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_2_14_TITLE_SELECTOR, form))))
            .setIsFrameworkAgreement(TenderNedTenderOldAndNewFormUtils.parseNewTenderIsFrameworkAgreement(form))
            .addPublication(TenderNedTenderOldAndNewFormUtils.parseNewTenderPreviousPublicationInTed(form))
            .setFinalPrice(parseTenderFinalPrice(form))
            .setLots(parseTenderLots(form))
            .setIsCoveredByGpa(TenderNedTenderOldAndNewFormUtils.parseIsTenderCoveredByGpa(form))
            .setSupplyType(TenderNedTenderOldAndNewFormUtils.parseTenderSupplyType(form))
            .setBuyerAssignedId(TenderNedTenderOldAndNewFormUtils.parseBuyerAssignedId(form));

        return parsedTender;
    }

    /**
     * Parse buyer main activity value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return non-empty list of activities or null
     */
    private static List<String> parseBuyerMainActivity(final Element form) {
        Elements nodes = JsoupUtils.select("ul > li", ParserUtils.getSubsectionOfElements(
            JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_I_5_TITLE_SELECTOR, form),
            JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_1_TITLE_SELECTOR, form)));

        if (nodes == null || nodes.isEmpty()) {
            return null;
        }

        List<String> activities = new ArrayList<>();
        nodes.forEach(n -> {
            activities.add(n.text());
        });

        return activities;
    }

    /**
     * Parse tender final price value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return tender final price or Null
     */
    private static ParsedPrice parseTenderFinalPrice(final Element form) {
        Element endElement = JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_8_TITLE_SELECTOR, form);
        if (endElement == null) {
            endElement = JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_2_TITLE_SELECTOR, form);
        }
        assert endElement != null;

        final Element subsection = ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_7_TITLE_SELECTOR, form),
                endElement);

        if (subsection == null) {
            return null;
        }
        final Elements subsectionChildren = subsection.children();
        if (subsectionChildren.size() == 2 && subsectionChildren.get(1).ownText().equals("-")) {
            return null;
        }

        // Examples of price (first row of subsection):
        // - "Waarde: 160 000,00"
        // - "Laagste offerte: 1,00 / Hoogste offerte: 2,00 meegerekend"

        final String amountString = "Waarde:";
        final String currencyString = "Munt:";
        final String amountRow = subsectionChildren.get(1).ownText();
        assert subsectionChildren.size() == 3 && subsectionChildren.get(2).ownText().startsWith(currencyString);
        if (amountRow.startsWith(amountString)) {
            return new ParsedPrice()
                    .setNetAmount(amountRow.substring(amountString.length()).trim())
                    .setCurrency(subsectionChildren.get(2).ownText().substring(currencyString.length()).trim());
        } else {
            return parseMinAndMaxNetAmount(amountRow, subsectionChildren.get(2).ownText());
        }
    }

    /**
     * Parse tender lots from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return List<ParsedTenderLot> or Null
     */
    private static List<ParsedTenderLot> parseTenderLots(final Element form) {
        List<ParsedTenderLot> lots;

        // some initial information are in subsection II.2
        lots = TenderNedTenderOldAndNewFormUtils.parseNewTenderLots(form);

        // and some awarded information are in section V. See
        // https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/b23fb9662492112db86ccef2c0c23a06
        // /pageId/D909A/huidigemenu/aankondigingen/cid/1273314/cvp/join

        List<Element> awardedLotSubsections = getAwardedLotsSubsections(form);
        if (awardedLotSubsections == null) {
            return lots;
        }

        for (Element awardedLotSubsection : awardedLotSubsections) {
            final Element titleElement = JsoupUtils.selectFirst("p", awardedLotSubsection);

            // get already created lot
            final String lotNumber = JsoupUtils.selectFirst("span:containsOwn(Perceel nr.:)", titleElement)
                    .nextSibling().toString().trim();
            ParsedTenderLot lot;
            if (lotNumber.equals("-")) {
                assert lots.size() == 1 : "Lot number is not entered when the tender has just one tender";
                lot = lots.get(0);
            } else {
                List<ParsedTenderLot> filteredLots = lots
                        .stream()
                        .filter(l -> l.getLotNumber() != null && l.getLotNumber().equals(lotNumber))
                        .collect(Collectors.toList());
                if  (filteredLots.size() == 1) {
                    lot = filteredLots.get(0);
                } else {
                    assert filteredLots.size() == 0 && lotNumber.equals("1") && lots.size() == 1
                            && lots.get(0).getLotNumber() == null;
                    lot = lots.get(0);
                }
            }

            Element descriptionListInV23 = JsoupUtils.selectFirst("h5:has(span:containsOwn(V.2.3)) + dl",
                awardedLotSubsection);
            lot.setContractNumber(JsoupUtils.selectFirst("span:containsOwn(Opdracht nr.:)", titleElement)
                    .nextSibling().toString().trim())
                .setAwardDecisionDate(JsoupUtils.selectText("h5:has(span:containsOwn(V.2.1)) + p",
                    awardedLotSubsection))
                .addBid(new ParsedBid()
                    .setIsWinning(Boolean.TRUE.toString())
                    .addBidder(new ParsedBody()
                        .setName(ParserUtils.getFromContent(descriptionListInV23,
                            "dt:containsOwn(Officiële benaming:) + dd", 0))
                        .addBodyId(TenderNedTenderFormUtils.parseBodyIdentifier(ParserUtils.getFromContent(
                            descriptionListInV23, "dl > dt:containsOwn(Nationale identificatie:) + dd", 0)))
                        .setAddress(new ParsedAddress()
                            .setStreet(ParserUtils.getFromContent(descriptionListInV23,
                                "dt:containsOwn(Postadres:) + dd", 0))
                            .setCity(ParserUtils.getFromContent(descriptionListInV23,
                                "dt:containsOwn(Plaats:) + dd", 0))
                            .addNuts(ParserUtils.getFromContent(descriptionListInV23,
                                "dt:containsOwn(NUTS-code:) + dd", 0))
                            .setPostcode(ParserUtils.getFromContent(descriptionListInV23,
                                "dt:containsOwn(Postcode:) + dd", 0))
                            .setCountry(ParserUtils.getFromContent(descriptionListInV23,
                                "dt:containsOwn(Land:) + dd", 0))
                            .setUrl(ParserUtils.getFromContent(descriptionListInV23,
                                "dt:containsOwn(Internetadres:) + dd", 0)))
                        .setEmail(ParserUtils.getFromContent(descriptionListInV23,
                            "dt:containsOwn(E-mail:) + dd", 0))));

            // set title only when it is not already set
            if (lot.getTitle() == null) {
                final String lotTitle = JsoupUtils.selectFirst("span:containsOwn(Benaming:)", titleElement)
                    .nextSibling().toString().trim();
                if (!lotTitle.equals("-")) {
                    lot.setTitle(lotTitle);
                }
            }

            // lot estimated price + bid price (it is in V.2.4)
            final String estimatedPriceTitle = "Aanvankelijk geraamde totale waarde van de opdracht/het perceel:";
            Element estimatedPriceElement = JsoupUtils.selectFirst("p:containsOwn(" + estimatedPriceTitle + ")",
                awardedLotSubsection);
            if (estimatedPriceElement != null) {
                // lot estimated price
                final String estimatedPriceNetAmount = estimatedPriceElement.ownText().substring(
                    estimatedPriceTitle.length()).trim();
                if (!estimatedPriceNetAmount.equals("-")) {
                    lot.setEstimatedPrice(new ParsedPrice()
                        .setNetAmount(estimatedPriceNetAmount));
                }

                // bid price
                Element bidPriceNetAmountElement = estimatedPriceElement.nextElementSibling();
                if (bidPriceNetAmountElement != null) {
                    final String bidNetAmountTitle = "Totale waarde van de opdracht/het perceel:";
                    final Element bidPriceCurrencyElement = bidPriceNetAmountElement.nextElementSibling();
                    if (bidPriceNetAmountElement.ownText().startsWith(bidNetAmountTitle)) {
                        final String currencyString = "Munt:";
                        assert bidPriceCurrencyElement.ownText().startsWith(currencyString);
                        lot.getBids().get(0)
                                .setPrice(new ParsedPrice()
                                        .setNetAmount(bidPriceNetAmountElement.ownText().substring(
                                                bidNetAmountTitle.length()).trim())
                                        .setCurrency(bidPriceCurrencyElement.ownText().substring(
                                                currencyString.length()).trim()));
                    } else {
                        lot.getBids().get(0)
                                .setPrice(parseMinAndMaxNetAmount(bidPriceNetAmountElement.ownText(),
                                        bidPriceCurrencyElement.ownText()));
                    }
                }
            }

            // bids count
            String bidsCountTitle = "Aantal inschrijvingen:";
            String bidsCountText = JsoupUtils.selectText("h5:has(span:containsOwn(V.2.2))"
                + " ~ p:containsOwn(" + bidsCountTitle + ")", awardedLotSubsection);
            if (bidsCountText != null) {                
                lot.setBidsCount(bidsCountText.substring(bidsCountTitle.length()).trim());
            }

            String isConsortiumTitle = "De opdracht is gegund aan een groep ondernemers:";
            String isConsortiumText = JsoupUtils.selectText("h5:has(span:containsOwn(V.2.2))"
                + " ~ p:containsOwn(" + isConsortiumTitle + ")", awardedLotSubsection);
            if (isConsortiumText != null) {
                lot.getBids().get(0).setIsConsortium(BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(
                    isConsortiumText.substring(isConsortiumTitle.length()))));
            }
            
            String electronicBidsCountTitle = "Aantal langs elektronische weg ontvangen inschrijvingen:";
            String electronicBidsCountText = JsoupUtils.selectText("h5:has(span:containsOwn(V.2.2))"
                + " ~ p:containsOwn(" + electronicBidsCountTitle + ")", awardedLotSubsection);
            if (electronicBidsCountText != null) {
                lot.setElectronicBidsCount(electronicBidsCountText.substring(electronicBidsCountTitle.length()));
            }

            String isAwrdedTitle = "Een opdracht/perceel wordt gegund:";
            String isAwrdedText = JsoupUtils.selectText("p:containsOwn(" + isAwrdedTitle + ")", awardedLotSubsection);
            if (isAwrdedText != null) {
                lot.setIsAwarded(BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(
                    isAwrdedText.substring(isAwrdedTitle.length()))));
            }
        }

        return lots;
    }

    /**
     * Parses price which is entered by minimum and maximum amount and by currency.
     *
     * @param amountRow
     *         string where minimum and maximum amounts are filled
     * @param currencyRow
     *         string where currency is filled
     *
     * @return parsed price or null
     */
    private static ParsedPrice parseMinAndMaxNetAmount(final String amountRow, final String currencyRow) {
        final String lowestOfferString = "Laagste offerte:";
        final String highesOfferString = "/ Hoogste offerte:";
        final String stringAtTheEnd = "meegerekend";
        final String currencyString = "Munt:";
        assert amountRow.startsWith(lowestOfferString) && amountRow.contains(highesOfferString)
                && amountRow.endsWith(stringAtTheEnd);
        return new ParsedPrice()
                .setMinNetAmount(amountRow.substring(lowestOfferString.length(),
                        amountRow.indexOf(highesOfferString)).trim())
                .setMaxNetAmount(amountRow.substring(
                        amountRow.indexOf(highesOfferString) + highesOfferString.length(),
                        amountRow.indexOf(stringAtTheEnd)).trim())
                .setCurrency(currencyRow.substring(currencyString.length()).trim());
    }

    /**
     * Creates list of elements where each element represents one lot.
     *
     * @param form
     *         document to be parsed
     *
     * @return List<Element> or null
     */
    private static List<Element> getAwardedLotsSubsections(final Element form) {
        List<Element> lotFirstLines = JsoupUtils.select("h3[id*='detail-publicatie:linkS5']", form);

        if (lotFirstLines.isEmpty()) {
            return null;
        }

        List<Element> subsections = new ArrayList<>();

        for (int i = 0; i < lotFirstLines.size(); i++) {
            if ((i + 1) != lotFirstLines.size()) {
                subsections.add(ParserUtils.getSubsectionOfElements(lotFirstLines.get(i),
                        lotFirstLines.get(i + 1)));
            } else {
                subsections.add(ParserUtils.getSubsectionOfElements(lotFirstLines.get(i), null));
            }
        }

        return subsections;
    }


}
