package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Base handler for parsing new forms "EU 3 - SL", "EU 6 - SL" and "NMV2".
 */
abstract class BaseENarocanjeContractAwardInDivsHandler extends BaseENarocanjeFormInDivsHandler {
    private static final Logger logger = LoggerFactory.getLogger(ENarocanjeContractAwardHandler1New.class);

    /**
     * Parses common contract award attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from form
     */
    public static ParsedTender parseCommonContractAwardAttributes(final ParsedTender tender, final Element form) {
        parseCommonFormInDivsAttributes(tender, form);

        final Element sectionII13 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_II_1_3_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_II_1_4_TITLE_SELECTOR, form));
        final Element sectionII17 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(SUBSECTION_II_1_7_TITLE_SELECTOR, form),
                JsoupUtils.selectFirst(SUBSECTION_II_2_TITLE_SELECTOR, form));

        tender
                .setFinalPrice(parseTenderFinalPrice(sectionII17))
                .setLots(parseAwardedLots(form, tender.getLots()))
                .setSupplyType(sectionII13.ownText());

        return tender;
    }

    /**
     * Parses all the awarded lots from document and join it with already parsed lots from section II.2.
     *
     * @param form
     *         parsed document for the source HTML page (parsed form)
     * @param alreadyParsedLots
     *         lots which were parsed from section II.2
     *
     * @return list of all parsed lots or empty list if no lots specified
     */
    static List<ParsedTenderLot> parseAwardedLots(final Element form, final List<ParsedTenderLot> alreadyParsedLots) {
        final Elements lotElements = JsoupUtils.select(
                "div.tab-content > div > h4:containsOwn(Oddelek V: Oddaja naročila) ~ div", form);

        if (lotElements.isEmpty()) {
            assert false : "Contract award should contain some awarded lot!";
            return alreadyParsedLots;
        }
        if (alreadyParsedLots.size() == 1 && alreadyParsedLots.get(0).getLotNumber() == null
                && lotElements.size() > 1) {
            // we are not able to match already parsed lot -> do not parse awarded lots
            // e.g http://www.enarocanje.si/Obrazci/?id_obrazec=156284
            return alreadyParsedLots;
        }

        final String lotTitleSelector = "h4:containsOwn(Sklop)";
        final String sectionV2TitleSelector = "h5:containsOwn(V.2" + ENarocanjeTenderParser.NBSP_CHARACTER
                + "Oddaja naročila)";
        final String sectionV21TitleSelector = "h5:containsOwn(V.2.1" + ENarocanjeTenderParser.NBSP_CHARACTER
                + "Datum sklenitve pogodbe)";
        final String sectionV22TitleSelector = "h5:containsOwn(V.2.2" + ENarocanjeTenderParser.NBSP_CHARACTER
                + "Informacije o ponudbah)";
        final String sectionV23TitleSelector = "h5:containsOwn(V.2.3" + ENarocanjeTenderParser.NBSP_CHARACTER
                + "Ime in naslov izvajalca)";
        final String sectionV23ContentSelector = sectionV23TitleSelector + " ~ div";
        final String sectionV24TitleSelector = "h5:containsOwn(V.2.4" + ENarocanjeTenderParser.NBSP_CHARACTER
                + "Informacije o vrednosti javnega naročila/sklopa)";
        final String sectionV25TitleSelector = "h5:containsOwn(V.2.5" + ENarocanjeTenderParser.NBSP_CHARACTER
                + "Informacije o naročilih, oddanih podizvajalcem)";

        for (Element lotElement : lotElements) {
            final Element sectionV22Title = JsoupUtils.selectFirst(sectionV22TitleSelector, lotElement);
            final Element sectionV23Title = JsoupUtils.selectFirst(sectionV23TitleSelector, lotElement);
            final Element titleSection = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(lotTitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionV2TitleSelector, lotElement));
            final Element sectionV21 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionV21TitleSelector, lotElement),
                    sectionV22Title == null ? sectionV23Title : sectionV22Title);
            final Element sectionV22 = ParserUtils.getSubsectionOfNodes(sectionV22Title, sectionV23Title);
            final Elements biddersInSectionV23 = JsoupUtils.select(sectionV23ContentSelector, lotElement);
            final Element sectionV24 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst(sectionV24TitleSelector, lotElement),
                    JsoupUtils.selectFirst(sectionV25TitleSelector, lotElement));

            // assert which checks that prices are without VAT
            assert sectionV24 == null || JsoupUtils.exists(sectionV24TitleSelector + " > small:containsOwn((brez DDV))",
                    lotElement);

            // get lot which will be set
            final ParsedTenderLot lotWhereToJoin;
            if (alreadyParsedLots.size() == 1 && lotElements.size() == 1) {
                // join the one lot from II.2 with one lot from V. Lot numbers are not always filled.
                // E.g. https://www.enarocanje.si/Obrazci/?id_obrazec=179439
                lotWhereToJoin = alreadyParsedLots.get(0);
            } else {
                // get lot number. At first we get lot number title in title section. Possibilities are
                // - https://www.enarocanje.si/Obrazci/?id_obrazec=207933
                // - https://www.enarocanje.si/Obrazci/?id_obrazec=179014
                final String lotNumber;
                final String lotNumberTitle = titleSection.html().contains(" Številka sklopa:")
                        ? " Številka sklopa:"
                        : "Številka sklopa:";
                final String lotNumberInTitleSection = ParserUtils.getFromContent(titleSection, null, lotNumberTitle);
                // try to get lot number from title when lot number from title section is not filled or
                // we have already parsed awarded lot with the lot number (we check it by estimated price, which is
                // parsed only in section V)
                if (lotNumberInTitleSection == null || alreadyParsedLots.stream()
                        .filter(l -> l.getEstimatedPrice() != null)
                        .anyMatch(l -> l.getLotNumber().equals(lotNumberInTitleSection))) {
                    // a different awarded lot with the same lot number has been already parsed, try lot number in title
                    final String lotNumberInTitle = ParserUtils.getFromContent(
                            JsoupUtils.selectFirst(lotTitleSelector, lotElement), null, "Sklop");
                    assert lotNumberInTitle != null;
                    if (alreadyParsedLots.stream()
                            .filter(l -> l.getEstimatedPrice() != null)
                            .anyMatch(l -> l.getLotNumber().equals(lotNumberInTitle))) {
                        logger.warn("Lot numbers in title section ({}) and in title ({}) are used by different " +
                                "awarded lot(s). The lot is skipped", lotNumberInTitleSection, lotNumberInTitle);
                        continue;
                    } else {
                        logger.warn(
                                "Lot number {} in title section is used by different awarded lot. " +
                                        "We use lot number {} in title", lotNumberInTitleSection, lotNumberInTitle);
                        lotNumber = lotNumberInTitle;
                    }
                } else {
                    lotNumber = lotNumberInTitleSection;
                }

                List<ParsedTenderLot> lotsWhereToJoin = alreadyParsedLots
                        .stream()
                        .filter(l -> l.getLotNumber().equals(lotNumber))
                        .collect(Collectors.toList());

                if (lotsWhereToJoin.isEmpty()) {
                    // Sometimes we have awarded lot (in section V) which is not in section II - see
                    // https://www.enarocanje.si/Obrazci/?id_obrazec=179901
                    lotWhereToJoin = new ParsedTenderLot()
                            .setLotNumber(lotNumber);
                    alreadyParsedLots.add(lotWhereToJoin);
                } else {
                    assert lotsWhereToJoin.size() == 1;
                    lotWhereToJoin = lotsWhereToJoin.get(0);
                }
            }

            assert lotWhereToJoin.getEstimatedPrice() == null
                    : "Lot in contract award should have estimated price only in section V";

            // get bids
            List<ParsedBid> bids = null;
            if (biddersInSectionV23 != null) {
                bids = new ArrayList<>();
                final ParsedPrice bidPrice = parsePrice(ParserUtils.getFromContent(sectionV24, null,
                        "Skupna vrednost naročila/sklopa:"), false);
                for (Element bidderInSectionV23 : biddersInSectionV23) {
                    final ParsedBody bidder = parseCommonBodyAttributes(bidderInSectionV23);
                    bidder.getAddress()
                            .setUrl(JsoupUtils.selectText("label:containsOwn(Internetni naslov (URL):) + a",
                                    bidderInSectionV23));
                    bidder.setIsSme(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                            JsoupUtils.selectOwnText("div > div > span:has(label:containsOwn(Izvajalec je MSP:))",
                                    bidderInSectionV23))));

                    bids.add(new ParsedBid()
                            .setIsWinning(Boolean.TRUE.toString())
                            .setPrice(bidPrice)
                            .addBidder(bidder)
                            .setIsConsortium(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                                    ParserUtils.getFromContent(sectionV22, null,
                                            "Naročilo je bilo oddano skupini gospodarskih subjektov:")))));
                }
            }

            lotWhereToJoin
                    .setIsAwarded(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                            ParserUtils.getFromContent(titleSection, null, "Naročilo je oddano/sklop je oddan:"))))
                    .setBidsCount(ParserUtils.getFromContent(sectionV22, null, "Število prejetih ponudb:"))
                    .setEstimatedPrice(parsePrice(ParserUtils.getFromContent(sectionV24, null,
                            "Začetna skupna ocenjena vrednost javnega naročila/sklopa:"), false))
                    .setBids(bids)
                    // section V.2.1 is not always presented. See
                    // https://www.enarocanje.si/Obrazci/?id_obrazec=213365
                    .setContractSignatureDate(sectionV21 == null ? null : sectionV21.ownText());
        }

        return alreadyParsedLots;
    }

    /**
     * Parse tender final price value from document.
     *
     * @param sectionII17
     *         html for section II.1.7
     *
     * @return tender final price or Null
     */
    private static ParsedPrice parseTenderFinalPrice(final Element sectionII17) {
        // assert which checks that price is without VAT
        assert JsoupUtils.exists("h5:containsOwn(II.1.7) > small:containsOwn((brez DDV))", sectionII17);

        return parsePrice(ParserUtils.getFromContent(sectionII17, null, "Vrednost:"), false);
    }

    /**
     * Parse price value.
     *
     * @param amountAndCurrency
     *         text with amount and currency
     * @param isWithVat
     *         whether the price is with or without price
     *
     * @return price or Null
     */
    static ParsedPrice parsePrice(final String amountAndCurrency, final boolean isWithVat) {
        if (amountAndCurrency == null) {
            return null;
        }

        // e.g. "983.606,56 EUR" or "EUR"

        if (amountAndCurrency.equals("EUR")) {
            return new ParsedPrice().setCurrency(amountAndCurrency);
        }

        assert amountAndCurrency.contains(" ") && amountAndCurrency.indexOf(' ') == amountAndCurrency.lastIndexOf(' ');
        final String[] amountAndCurrencyArray = amountAndCurrency.split(" ");

        final ParsedPrice price = new ParsedPrice();
        if (isWithVat) {
            price.setAmountWithVat(amountAndCurrencyArray[0]);
        } else {
            price.setNetAmount(amountAndCurrencyArray[0]);
        }
        return price.setCurrency(amountAndCurrencyArray[1]);
    }

}
