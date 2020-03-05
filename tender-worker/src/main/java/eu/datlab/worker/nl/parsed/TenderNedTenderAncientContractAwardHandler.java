package eu.datlab.worker.nl.parsed;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.jsoup.nodes.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static eu.datlab.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_II_1_2_CONTENT_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_II_1_4_CONTENT_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_II_1_5_CONTENT_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_II_1_6_CONTENT_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_VI_1_CONTENT_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_VI_3_1_1_CONTENT_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_VI_3_1_2_CONTENT_SELECTOR;
import static eu.datlab.worker.nl.parsed.TenderNedTenderAncientFormUtils.ANCIENT_SUBSECTION_V_FIRST_ELEMENT_OF_FIRST_LOT_SELECTOR;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Parser for TenderNed ancient contract award form specific data.
 *
 * @author Marek Mikes
 */
final class TenderNedTenderAncientContractAwardHandler {
    private static final Logger logger = LoggerFactory.getLogger(TenderNedTenderAncientContractAwardHandler.class);

    /**
     * Private constructor to make this class static.
     */
    private TenderNedTenderAncientContractAwardHandler() {
    }

    /**
     * Parse method for TenderNed contract award form from specific data.
     *
     * @param parsedTender
     *         tender to add data to
     * @param form
     *         document to parse data from
     *
     * @return ParsedTender with data added
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Element form) {
        return parsedTender
            .addBuyer(TenderNedTenderAncientFormUtils.parseFirstTenderBuyer(form))
            .addBuyer(TenderNedTenderAncientFormUtils.parseSecondTenderBuyer(form))
            .setSupplyType(TenderNedTenderAncientFormUtils.parseTenderSupplyType(form))
            .setOnBehalfOf(TenderNedTenderAncientFormUtils.parseTenderOnBehalfOf(form))
            .setTitle(TenderNedTenderAncientFormUtils.parseTenderTitle(form))
            .setAddressOfImplementation(new ParsedAddress()
                .setRawAddress(parseRawAddressOfImplementation(form))
                .addNuts(TenderNedTenderAncientFormUtils.parseAddressOfImplementationNuts(form)))
            .setIsFrameworkAgreement(TenderNedTenderAncientFormUtils.parseIsTenderFrameworkAgreement(form))
            .setDescription(TenderNedTenderAncientFormUtils.parseTenderDescription(form,
                ANCIENT_SUBSECTION_II_1_4_CONTENT_SELECTOR))
            .setCpvs(TenderNedTenderAncientFormUtils.parseTenderCpvs(form, ANCIENT_SUBSECTION_II_1_5_CONTENT_SELECTOR))
            .setIsCoveredByGpa(parseIsTenderCoveredByGpa(form))
            .setNationalProcedureType(TenderNedTenderAncientFormUtils.parseTenderNationalProcedureType(form))
            .setAwardCriteria(TenderNedTenderAncientFormUtils.parseAwardCriteria(form))
            .setIsElectronicAuction(TenderNedTenderAncientFormUtils.parseIfTenderIsElectronicAuction(form))
            .setBuyerAssignedId(TenderNedTenderAncientFormUtils.parseTenderBuyerAssignedId(form))
            .addFunding(new ParsedFunding().setIsEuFund(parseIsEuFunded(form)))
            .setAppealBodyName(parseTenderAppealBodyName(form))
            .setMediationBodyName(parseTenderMediationBodyName(form))
            .setLots(parseTenderLots(form))
            .setSelectionMethod(TenderNedTenderAncientFormUtils.parseSelectionMethod(form));
    }

    /**
     * Parse raw address of implementation from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseRawAddressOfImplementation(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_II_1_2_CONTENT_SELECTOR,
                "Belangrijkste plaats van dienstverlening:");
    }

    /**
     * Parse if tender is covered by GPA.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseIsTenderCoveredByGpa(final Element form) {
        return BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(
                ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_II_1_6_CONTENT_SELECTOR, 0)));
    }

    /**
     * Parse if source is from EU funds from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseIsEuFunded(final Element form) {
        return BooleanUtils.toStringTrueFalse(TenderNedTenderFormUtils.meansYes(
                ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_VI_1_CONTENT_SELECTOR, 0)));
    }

    /**
     * Parse tender appeal body name value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderAppealBodyName(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_VI_3_1_1_CONTENT_SELECTOR, 0);
    }

    /**
     * Parse tender mediation body name value from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderMediationBodyName(final Element form) {
        return ParserUtils.getFromContent(form, ANCIENT_SUBSECTION_VI_3_1_2_CONTENT_SELECTOR, 0);
    }

    /**
     * Parse tender lots from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return list of lots or Null
     */
    private static List<ParsedTenderLot> parseTenderLots(final Element form) {
        final String sectionFooterClassName = "section-footer";

        Element lotElement = JsoupUtils.selectFirst(ANCIENT_SUBSECTION_V_FIRST_ELEMENT_OF_FIRST_LOT_SELECTOR, form);
        if (lotElement == null || lotElement.className().equals(sectionFooterClassName)) {
            return null;
        }

        // sometime some subsection does not exist:
        // - V.1, V.2 and V.5:
        //   https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
        // d33ad51ba567ecc985cbbb187958e761/pageId/D909A/huidigemenu/aankondigingen/cid/850822/cvp/join)
        // - V.5:
        //   https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
        //   002ee7d59bb6d1dae5747b2023f95390/pageId/D909A/huidigemenu/aankondigingen/cid/808319/cvp/join)

        List<ParsedTenderLot> lots = new ArrayList<>();
        final String lotNumberStartText = "PERCEEL nr.";
        final String lotTitleStartText = "BENAMING:";
        do {
            ParsedTenderLot lot = new ParsedTenderLot();

            String[] lotNumberAndTitle = lotElement.html().split("<br>");
            for (String row : lotNumberAndTitle) {
                if (row.startsWith(lotNumberStartText)) {
                    lot.setLotNumber(row.substring(lotNumberStartText.length()).trim());
                } else if (row.startsWith(lotTitleStartText)) {
                    lot.setTitle(row.substring(lotTitleStartText.length()).trim());
                }
            }
            lotElement = lotElement.nextElementSibling();

            // subsection V.1
            if (lotElement.text().startsWith("V.1")) {
                lotElement = lotElement.nextElementSibling();
                lot.setAwardDecisionDate(lotElement.ownText().trim());
                lotElement = lotElement.nextElementSibling();
            }
            // subsection V.2
            if (lotElement.text().startsWith("V.2")) {
                lotElement = lotElement.nextElementSibling();
                String[] subsection2Rows = lotElement.html().split("<br>");
                // subsection V.2 should contain bids count and electronic bids count (two rows), otherwise it is bug on
                // the form (it is in https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
                // 002ee7d59bb6d1dae5747b2023f95390/pageId/D909A/huidigemenu/aankondigingen/cid/808319/cvp/join)
                if (subsection2Rows.length == 2) {
                    lot.setBidsCount(subsection2Rows[0].substring("Aantal ontvangen inschrijvingen:".length()).trim());
                    String electronicBidsCount = subsection2Rows[1]
                            .substring("Aantal langs elektronische weg ontvangen inschrijvingen:".length()).trim();
                    lot.setElectronicBidsCount(electronicBidsCount.equals("-") ? null : electronicBidsCount);
                }
                lotElement = lotElement.nextElementSibling();
            }
            // subsection V.3
            if (lotElement.text().startsWith("V.3")) {
                lotElement = lotElement.nextElementSibling();
                String[] subsection3Rows = lotElement.html().split("<br>");
                lot.addBid(new ParsedBid()
                    .setIsWinning(Boolean.TRUE.toString())
                    .addBidder(new ParsedBody().setName(subsection3Rows[0].trim())));

                if (subsection3Rows.length > 1) {
                    int addresStartIndex = 1;
                    // check whether the row includes oraganization id               
                    if (subsection3Rows[1].trim().matches("(NL)?\\d+(B\\d{2})?")) {
                        lot.getBids().get(0).getBidders().get(0)
                            .addBodyId(TenderNedTenderFormUtils.parseBodyIdentifier(subsection3Rows[1]));

                        addresStartIndex++;
                    }

                    // raw address is the rest of content of the subsection V.3
                    final String rawAddress = String.join("", Arrays.copyOfRange(subsection3Rows, addresStartIndex,
                        subsection3Rows.length));

                    lot.getBids().get(0).getBidders().get(0)
                        .setAddress(new ParsedAddress().setRawAddress(rawAddress.trim()));
                }
                lotElement = lotElement.nextElementSibling();
            }
            // subsection V.4
            if (lotElement.text().startsWith("V.4")) {
                // Example price format in one row: "Waarde: 1268001 Munt: EUR  Met BTW BTW-tarief (%): 19
                lotElement = lotElement.nextElementSibling();
                if (lotElement.hasText()) {
                    // E.g. https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
                    // fda3cec475388d80658e0d41d14e0e52/pageId/D909A/huidigemenu/aankondigingen/cid/1086568/cvp/join
                    String[] subsection4Rows = lotElement.html().split("<br>");
                    for (int i = 0; i < subsection4Rows.length; ++i) {
                        if (subsection4Rows[i].startsWith("Aanvankelijke geraamde totale waarde van de opdracht:")) {
                            ++i;
                            lot.setEstimatedPrice(parsePriceFromRow(subsection4Rows[i]));
                        } else if (subsection4Rows[i].startsWith("Totale definitieve waarde van de opdracht:")) {
                            ++i;
                            if (lot.getBids() == null) {
                                lot.addBid(new ParsedBid()
                                        .setIsWinning(Boolean.TRUE.toString()));
                            }
                            lot.getBids().get(0).setPrice(parsePriceFromRow(subsection4Rows[i]));
                        }
                    }
                // class of next sibling element has to be "subsection" - it filters "Go to top" element and
                // first element of next lot
                } else if (!lotElement.nextElementSibling().text().startsWith("V.5")
                        && lotElement.nextElementSibling().className().equals("subsection")) {
                    // E.g. https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
                    // 02e418d4a8ca69507d2a5ce2bc673098/pageId/D909A/huidigemenu/aankondigingen/cid/931844/cvp/join
                    lotElement = lotElement.nextElementSibling();
                    assert lotElement.text().equals("Aanvankelijke geraamde totale waarde van de opdracht");
                    lotElement = lotElement.nextElementSibling();
                    lot.setEstimatedPrice(parsePriceFromRows(lotElement));
                    lotElement = lotElement.nextElementSibling();
                    assert lotElement.text().equals("Totale definitieve waarde van de opdracht");
                    lotElement = lotElement.nextElementSibling();
                    if (lot.getBids() == null) {
                        lot.addBid(new ParsedBid()
                                .setIsWinning(Boolean.TRUE.toString()));
                    }
                    lot.getBids().get(0).setPrice(parsePriceFromRows(lotElement));
                }
                lotElement = lotElement.nextElementSibling();
            }
            // subsection V.5
            if (lotElement.text().startsWith("V.5")) {
                lotElement = lotElement.nextElementSibling();
                if (lot.getBids() == null) {
                    lot.addBid(new ParsedBid()
                            .setIsWinning(Boolean.TRUE.toString()));
                }
                // the element contains "neen" or something like
                // "ja Niet bekend: ja Korte beschrijving van de waarde/het deel van de opdracht dat wordt
                // uitbesteed: 50% van de spoedzorg", so we want the first string
                lot.getBids().get(0)
                        .setIsSubcontracted(lotElement.ownText().split(" ")[0]);
                lotElement = lotElement.nextElementSibling();
            }

            lots.add(lot);

            if (lotElement == null || lotElement.className().equals(sectionFooterClassName)) {
                break;
            }
        } while (true);
        return lots;
    }

    /**
     * Parse price from row.
     *
     * @param priceInformation
     *         string where all information are presented
     *
     * @return parsed price
     */
    private static ParsedPrice parsePriceFromRow(final String priceInformation) {
        String priceInformationTrimmed = priceInformation.trim();
        final String excludingVATString = "Zonder BTW";
        final String currencyString = "Munt:";
        if (priceInformationTrimmed.startsWith("Laagste offerte:")) {
            // examples of this notation are:
            // - "Laagste offerte: 173400 en hoogste offerte: 223995 Munt: EUR Met BTW BTW-tarief (%): 21"
            // - "Laagste offerte: 593000 en hoogste offerte: 774000 Munt: EUR Zonder BTW"
            String[] priceParts = priceInformationTrimmed.split(" ");
            if (priceInformationTrimmed.contains("Met BTW")) { // including VAT
                assert priceParts.length == 14 && priceParts[7].equals(currencyString)
                        && priceParts[11].equals("BTW-tarief");
                return new ParsedPrice()
                        .setMinAmountWithVat(priceParts[2])
                        .setMaxAmountWithVat(priceParts[6])
                        .setCurrency(priceParts[8])
                        .setVat(priceParts[13]);
            } else {
                assert priceInformationTrimmed.endsWith(excludingVATString);
                assert priceParts.length == 11 && priceParts[4].equals("hoogste")
                        && priceParts[7].equals(currencyString);
                return new ParsedPrice()
                        .setMinNetAmount(priceParts[2])
                        .setMaxNetAmount(priceParts[6])
                        .setCurrency(priceParts[8]);
            }
        } else if (priceInformationTrimmed.startsWith("Waarde:")) {
            // examples of this notation are:
            // - "Waarde: 253470 Munt: EUR Met BTW BTW-tarief (%): 19"
            // - "Waarde: 447458 Munt: EUR Zonder BTW"
            String[] priceParts = priceInformationTrimmed.split(" ");
            if (priceInformationTrimmed.contains("Met BTW")) { // including VAT
                assert priceParts.length == 9 && priceParts[2].equals(currencyString)
                        && priceParts[6].equals("BTW-tarief");
                return new ParsedPrice()
                        .setAmountWithVat(priceParts[1])
                        .setCurrency(priceParts[3])
                        .setVat(priceParts[8]);
            } else {
                assert priceInformationTrimmed.endsWith(excludingVATString);
                assert priceParts.length == 6 && priceParts[2].equals(currencyString);
                return new ParsedPrice()
                        .setNetAmount(priceParts[1])
                        .setCurrency(priceParts[3]);
            }
        } else if (priceInformationTrimmed.isEmpty() || priceInformationTrimmed.equals("-")) {
            return null;
        } else {
            logger.error("Unknown date string to parse. The string is {}.", priceInformationTrimmed);
            throw new UnrecoverableException("Unknown date string to parse.");
        }
    }

    /**
     * Parse price from rows.
     *
     * @param element
     *         element where each information is on one row
     *
     * @return parsed price
     */
    private static ParsedPrice parsePriceFromRows(final Element element) {
        final String amountString = "Waarde:";
        final String currencyString = "Munt:";
        final String vatString = "BTW-tarief (%):";
        String[] priceRows = element.html().split("<br>");

        // we have to decide whether price is with or without VAT. The information is not always on the last row. See
        // https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/96e7dcd71060397d0b342ac4fef9ce5e/
        // pageId/D909A/huidigemenu/aankondigingen/cid/949189/cvp/join
        Boolean priceIsWithVat = null;
        for (int i = 0; i < priceRows.length; ++i) {
            if (priceRows[i].startsWith("Met BTW")) {
                priceIsWithVat = true;
                break;
            }
            if (priceRows[i].startsWith("Zonder BTW")) {
                priceIsWithVat = false;
                break;
            }
        }

        // parse price without VAT when we do not know whether it is or is not with VAT
        if (priceIsWithVat == null) {
            priceIsWithVat = false;
        }

        ParsedPrice price = new ParsedPrice();
        if (priceIsWithVat) {
            for (int i = 0; i < priceRows.length; ++i) {
                if (priceRows[i].startsWith(amountString)) {
                    price.setAmountWithVat(priceRows[i].substring(amountString.length()).trim());
                } else if (priceRows[i].startsWith(currencyString)) {
                    price.setCurrency(priceRows[i].substring(currencyString.length()).trim());
                } else if (priceRows[i].startsWith(vatString)) {
                    price.setVat(priceRows[i].substring(vatString.length()).trim());
                }
            }
        } else {
            for (int i = 0; i < priceRows.length; ++i) {
                if (priceRows[i].startsWith(amountString)) {
                    price.setNetAmount(priceRows[i].substring(amountString.length()).trim());
                } else if (priceRows[i].startsWith(currencyString)) {
                    price.setCurrency(priceRows[i].substring(currencyString.length()).trim());
                }
            }
        }
        return price;
    }
}
