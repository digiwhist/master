package eu.digiwhist.worker.nl.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;

import java.util.ArrayList;
import java.util.List;

import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_II_1_5_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_II_1_6_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_VI_1_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SUBSECTION_VI_2_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils
        .OLD_AND_NEW_SECTION_V_FIRST_ELEMENT_OF_FIRST_LOT_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_I_3_TITLE_SELECTOR;
import static eu.digiwhist.worker.nl.parsed.TenderNedTenderOldAndNewFormUtils.OLD_AND_NEW_SUBSECTION_I_4_TITLE_SELECTOR;
import org.jsoup.select.Elements;

/**
 * Parser for TenderNed old contract award form specific data.
 *
 * @author Marek Mikes
 */
final class TenderNedTenderOldContractAwardHandler {
    /**
     * Private constructor to make this class static.
     */
    private TenderNedTenderOldContractAwardHandler() {
    }

    /**
     * Parse method for TenderNed old contract award form from specific data.
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
            .setUrl(TenderNedTenderOldAndNewFormUtils.parseOldBuyerUrl(form));

        parsedTender.getBuyers().get(0)
            .setBuyerType(TenderNedTenderOldAndNewFormUtils.parseOldTenderBuyerType(form))
            .setMainActivities(parseBuyerMainActivity(form));

        parsedTender
            .setDocumentsLocation(new ParsedAddress()
                .setUrl(TenderNedTenderOldAndNewFormUtils.parseOldTenderDocumentsUrl(form)))
            .setAddressOfImplementation(TenderNedTenderOldAndNewFormUtils.parseOldTenderAddressOfImplementation(form))
            .setIsFrameworkAgreement(TenderNedTenderOldAndNewFormUtils.parseOldTenderIsFrameworkAgreement(form))
            .setCpvs(TenderNedTenderOldAndNewFormUtils.parseOldTenderCpvs(ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_5_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_II_1_6_TITLE_SELECTOR, form))))
            .addFunding(TenderNedTenderOldAndNewFormUtils.parseTenderFunding(ParserUtils.getSubsectionOfElements(
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_VI_1_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_VI_2_TITLE_SELECTOR, form))))
            .addPublications(TenderNedTenderOldAndNewFormUtils.parseOldTenderPreviousPublicationsInTed(form))
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
            JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_I_3_TITLE_SELECTOR, form),
            JsoupUtils.selectFirst(OLD_AND_NEW_SUBSECTION_I_4_TITLE_SELECTOR, form)));

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
     * Parse tender lots from document.
     *
     * @param form
     *         document to be parsed
     *
     * @return list of lots or Null
     */
    private static List<ParsedTenderLot> parseTenderLots(final Element form) {
        final String sectionFooterClassName = "section-footer";

        Element lotElement = JsoupUtils.selectFirst(OLD_AND_NEW_SECTION_V_FIRST_ELEMENT_OF_FIRST_LOT_SELECTOR, form);
        if (lotElement == null) {
            return null;
        }

        List<ParsedTenderLot> lots = new ArrayList<>();
        final String lotNumberText = "Perceel nr";
        do {
            lotElement = lotElement.nextElementSibling();

            ParsedTenderLot lot = new ParsedTenderLot();
            if (!lotElement.nodeName().equals("h4")) {
                String lotFormTitle = lotElement.text();
                int colonIndex = lotFormTitle.indexOf(':');

                assert lotFormTitle.startsWith(lotNumberText) && colonIndex != -1;

                lot
                    .setTitle(lotFormTitle.substring(colonIndex + 1).trim())
                    .setLotNumber(lotFormTitle.substring(lotNumberText.length(), colonIndex).trim());

                lotElement = lotElement.nextElementSibling();
            }
            // subsection 1
            if (lotElement.text().startsWith("V.1")) {
                lotElement = lotElement.nextElementSibling();
                if (!lotElement.text().equals("-")) {
                    lot.setAwardDecisionDate(lotElement.text());
                }
                lotElement = lotElement.nextElementSibling();
            }
            // subsection 2
            if (lotElement.text().startsWith("V.2")) {
                lotElement = lotElement.nextElementSibling();
                assert lotElement.text().contains("Aantal ontvangen inschrijvingen:");
                lot.setBidsCount(lotElement.text().replace("Aantal ontvangen inschrijvingen:", "").trim());


                lotElement = lotElement.nextElementSibling();
                assert lotElement.text().contains("Aantal ontvangen inschrijvingen:");
                lot.setElectronicBidsCount(lotElement.text().replace("Aantal ontvangen inschrijvingen:", "").trim());
                
                lotElement = lotElement.nextElementSibling();
            }
            // subsection 3
            if (lotElement.text().startsWith("V.3")) {
                lotElement = lotElement.nextElementSibling();
                assert lotElement.nodeName().equals("dl");
                lot.addBid(new ParsedBid()
                    .setIsWinning(Boolean.TRUE.toString())
                    .addBidder(new ParsedBody()
                        .setName(ParserUtils.getFromContent(lotElement, "dt:containsOwn(Officiële benaming:) + dd", 0))
                        .setAddress(new ParsedAddress()
                            .setStreet(ParserUtils.getFromContent(lotElement, "dt:containsOwn(Postadres:) + dd", 0))
                            .setCity(ParserUtils.getFromContent(lotElement, "dt:containsOwn(Plaats:) + dd", 0))
                            .setPostcode(ParserUtils.getFromContent(lotElement, "dt:containsOwn(Postcode:) + dd", 0))
                            .setCountry(ParserUtils.getFromContent(lotElement, "dt:containsOwn(Land:) + dd", 0))
                            .setUrl(ParserUtils.getFromContent(lotElement, "dt:containsOwn(Internetadres:) + dd", 0)))
                        .setEmail(ParserUtils.getFromContent(lotElement, "dt:containsOwn(E-mail:) + dd", 0))));
                lotElement = lotElement.nextElementSibling();
            }
            // subsection 4
            if (lotElement.text().startsWith("V.4")) {
                lotElement = lotElement.nextElementSibling();
                if (!lotElement.text().equals("-")) {
                    assert lotElement.text().contains("Aanvankelijke geraamde totale waarde van de opdracht");
                    lotElement = lotElement.nextElementSibling();
                    lot.setEstimatedPrice(parsePrice(lotElement.text()));
                    lotElement = lotElement.nextElementSibling();
                    
                    assert lotElement.text().contains("Totale definitieve waarde van de opdracht");
                    lotElement = lotElement.nextElementSibling();
                    lot.getBids().get(0).setPrice(parsePrice(lotElement.text()));
                }
                lotElement = lotElement.nextElementSibling();
            }

            lots.add(lot);

            // move to the first element of next lot or to the end element of lots
            while (lotElement != null && !lotElement.text().contains("Opdracht nr")
                    && !lotElement.className().equals(sectionFooterClassName)) {
                lotElement = lotElement.nextElementSibling();
            }

            if (lotElement == null || lotElement.className().equals(sectionFooterClassName)) {
                break;
            }
        } while (true);
        return lots;
    }

    /**
     * Parse price value from string.
     *
     * @param priceString
     *         text to be parsed
     *
     * @return parsed price or Null
     */
    private static ParsedPrice parsePrice(final String priceString) {
        if (priceString == null) {
            return null;
        }
        String priceStringTrimmed = priceString.trim();
        if (priceStringTrimmed.equals("-")) {
            return null;
        }

        final String amountString = "Waarde:";
        final String vatIncludedText = "(Met btw meegerekend)";
        final String currencyString = "Munt:";
        final String vatString = "Btw-tarief:";
        final String lowestOfferString = "Laagste offerte";
        if (priceStringTrimmed.startsWith(lowestOfferString)) {
            // examples of this notation are:
            // - "Laagste offerte 73,00 en hoogste offerte 95,00 (Zonder btw meegerekend) Munt: EUR"
            // - "Laagste offerte 10 000,00 en hoogste offerte 100 000,00 Munt: EUR"
            final String andHighesOfferString = "en hoogste offerte";
            assert priceStringTrimmed.contains(andHighesOfferString);
            final int indexAfterMaxNetAmount = priceStringTrimmed.indexOf('(') != -1
                    ? priceStringTrimmed.indexOf('(')
                    : priceStringTrimmed.indexOf(currencyString);
            return new ParsedPrice()
                    .setMinNetAmount(priceStringTrimmed.substring(lowestOfferString.length(),
                            priceStringTrimmed.indexOf(andHighesOfferString)).trim())
                    .setMaxNetAmount(priceStringTrimmed.substring(
                            priceStringTrimmed.indexOf(andHighesOfferString) + andHighesOfferString.length(),
                            indexAfterMaxNetAmount).trim())
                    .setCurrency(priceStringTrimmed.substring(
                            priceStringTrimmed.indexOf(currencyString) + currencyString.length()).trim());
        } else if (priceStringTrimmed.startsWith("Waarde:")) {
            // examples of this notation are:
            // - "Waarde: 1,00 (Met btw meegerekend) Munt: EUR Btw-tarief: 21%"
            // - "Waarde: 600 000,00 (Zonder btw meegerekend) Munt: EUR"
            // - "Waarde: 285 000,00 Munt: EUR"
            if (priceString.contains(vatIncludedText)) {
                assert priceString.startsWith(amountString);
                return new ParsedPrice()
                        .setAmountWithVat(priceString.substring(amountString.length(), priceString.indexOf('(')).trim())
                        .setCurrency(priceString.substring(
                                priceString.indexOf(currencyString) + currencyString.length(),
                                priceString.indexOf(vatString)).trim())
                        .setVat(priceString.substring(priceString.indexOf(vatString) + vatString.length()).trim());
            } else {
                // price is without VAT

                ParsedPrice parsedPrice = new ParsedPrice();
                if (priceString.contains("(")) {
                    // price is something like "Waarde: 600 000,00 (Zonder btw meegerekend) Munt: EUR"
                    parsedPrice
                            .setNetAmountEur(priceString.substring(amountString.length(), priceString.indexOf('('))
                                    .trim());
                } else {
                    // price is something like "Waarde: 285 000,00 Munt: EUR"
                    parsedPrice
                            .setNetAmountEur(priceString.substring(amountString.length(),
                                    priceString.indexOf(currencyString)).trim());
                }

                return parsedPrice
                        .setCurrency(priceString.substring(priceString.indexOf(currencyString)
                                + currencyString.length()).trim());
            }
        } else {
            // price is something like "1 300 000,00"
            return new ParsedPrice()
                    .setNetAmountEur(priceString);
        }
    }

}
