package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Handler for parsing forms "EU 18 - SL", "PZPPO2 - ZJN-2" and "PZPPO2 - ZJNVETPS".
 */
final class ENarocanjeContractAwardHandler2 extends BaseENarocanjeFormInTableHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractAwardHandler2() {}

    private static final String sectionVTitle = "ODDELEK V: DOPOLNILNE INFORMACIJE";

    /**
     * Parses Form specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from Form
     */
    public static ParsedTender parse(final ParsedTender tender, final Element form) {
        parseCommonAttributes(tender, form);

        final Element sectionII21 = getSectionWithSeparatedNodes("II.2.1", form);
        // I can not call getSectionWithSeparatedNodes, because it would return section IV.1, not V.1
        final Element sectionV1 = ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst("div.tab-content > table > tbody > tr:contains(" + sectionVTitle +
                        ") ~ tr:contains(V.1) > *:first-child", form),
                null);

        tender
                .setTitle(JsoupUtils.selectText(String.format(SECTION_SELECTOR_PATTERN, "II.1.1"), form))
                .setFinalPrice(parseTenderFinalPrice(sectionII21))
                .setLots(parseLots(form))
                .setCpvs(parseTenderCpvs("II.1.5", form))
                .setFundings(parseFundings(sectionV1));

        return tender;
    }

    /**
     * Parse tender final price value from document.
     *
     * @param sectionII21
     *         html for section II.2.1
     *
     * @return tender final price or Null
     */
    private static ParsedPrice parseTenderFinalPrice(final Element sectionII21) {
        return parsePrice(ParserUtils.getFromContent(sectionII21, null, " Vrednost:"));
    }

    /**
     * Parses all the lots from document.
     *
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return list of all parsed lots or empty list if no lots specified
     */
    private static List<ParsedTenderLot> parseLots(final Element form) {
        final Elements lotTitleElements = getSections("ODDELEK IV: ODDAJA NAROČILA", form);
        if (lotTitleElements.isEmpty()) {
            return null;
        }

        // get elements representing lots
        final List<Element> lotElements = new ArrayList<>();
        final Element sectionVElement = getSection(sectionVTitle, form);
        for (int i = 0; i < lotTitleElements.size() - 1; ++i) {
            lotElements.add(ParserUtils.getSubsectionOfElements(lotTitleElements.get(i), lotTitleElements.get(i + 1)));
        }
        lotElements.add(ParserUtils.getSubsectionOfElements(lotTitleElements.get(lotTitleElements.size() - 1),
                sectionVElement));

        List<ParsedTenderLot> lots = new ArrayList<>();

        for (Element lotElement : lotElements) {
            final String lotNumberTitle = "ŠT. SKLOPA:";
            final String lotTitleTitle = "NAZIV:";
            final Element lotNumberRow = ParserUtils.getSubsectionOfNodes(JsoupUtils.selectFirst(
                    "tr:contains(" + lotNumberTitle + ") > *:first-child", lotElement), null);
            final Element lotTitleRow = ParserUtils.getSubsectionOfNodes(JsoupUtils.selectFirst(
                    "tr:contains(" + lotTitleTitle + ") > *:first-child", lotElement), null);
            final Element sectionIV2 = ParserUtils.getSubsectionOfNodes(JsoupUtils.selectFirst(
                    "tr:contains(IV.2) > *:first-child", lotElement), null);
            final Element sectionIV3 = ParserUtils.getSubsectionOfNodes(JsoupUtils.selectFirst(
                    "tr:contains(IV.3) > *:first-child", lotElement), null);
            final Element sectionIV4 = ParserUtils.getSubsectionOfNodes(JsoupUtils.selectFirst(
                    "tr:contains(IV.4) > *:first-child", lotElement), null);

            ParsedTenderLot lot = null;

            if (lotNumberRow != null) {
                lot = new ParsedTenderLot()
                        .setLotNumber(ParserUtils.getFromContent(lotNumberRow, null, lotNumberTitle));
            }

            if (lotTitleRow != null) {
                if (lot == null) {
                    lot = new ParsedTenderLot();
                }
                lot
                        .setTitle(ParserUtils.getFromContent(lotTitleRow, null, lotTitleTitle));
            }

            if (sectionIV2 != null) {
                if (lot == null) {
                    lot = new ParsedTenderLot();
                }
                final String bidsCount;
                final String bidsCountTitleInContent = " Število prejetih ponudb:";
                final String sectionIV2Text = sectionIV2.text();
                if (sectionIV2Text.contains(bidsCountTitleInContent)) {
                    bidsCount = ParserUtils.getFromContent(sectionIV2, null, bidsCountTitleInContent);
                } else {
                    // e.g. https://www.enarocanje.si/Obrazci/?id_obrazec=127347
                    final String bidsCountTitleInTitle = "Število prejetih ponudb";
                    bidsCount = sectionIV2Text.substring(sectionIV2Text.indexOf(bidsCountTitleInTitle) +
                            bidsCountTitleInTitle.length());
                }
                lot
                        .setBidsCount(bidsCount);
            }

            if (sectionIV3 != null) {
                if (lot == null) {
                    lot = new ParsedTenderLot();
                }
                if (lot.getBids() == null) {
                    lot
                            .addBid(new ParsedBid()
                                    .setIsWinning(Boolean.TRUE.toString()));
                }

                // row index (6) is checked from observation (it is the first row in the section)
                final String nameAndAddress = ParserUtils.getFromContent(sectionIV3, null, 6);

                parseNameAndAddressOfWinningBidder(nameAndAddress, lot);
            }

            if (sectionIV4 != null) {
                if (lot == null) {
                    lot = new ParsedTenderLot();
                }
                if (lot.getBids() == null) {
                    lot
                            .addBid(new ParsedBid()
                                    .setIsWinning(Boolean.TRUE.toString()));
                }

                if (sectionIV4.text().contains("Vrednost:")) {
                    // we have only winning price. E.g. https://www.enarocanje.si/Obrazci/?id_obrazec=127041
                    lot.getBids().get(0)
                            .setPrice(parsePrice(ParserUtils.getFromContent(sectionIV4, null, " Vrednost:")));
                } else {
                    // there is estimated price and winning price. E.g.
                    // https://www.enarocanje.si/Obrazci/?id_obrazec=133481
                    lot
                            .setEstimatedPrice(parsePrice(ParserUtils.getFromContent(sectionIV4, null,
                                    " Začetna skupna ocenjena vrednost naročila:")))
                            .getBids().get(0)
                                    .setPrice(parsePrice(ParserUtils.getFromContent(sectionIV4, null,
                                            "Skupna končna vrednost naročila:")));
                }
            }

            if (lot != null) {
                lots.add(lot);
            }
        }

        return lots.isEmpty() ? null : lots;
    }

    /**
     * Parses list of fundings.
     *
     * @param sectionV1
     *         html for section V.1
     *
     * @return non-empty list of fundings or null
     */
    private static List<ParsedFunding> parseFundings(final Element sectionV1) {
        if (sectionV1 == null) {
            // e.g. https://www.enarocanje.si/Obrazci/?id_obrazec=133622
            return null;
        }

        final String sectionV1PossibleTitle =
                "Naročilo se nanaša na projekt in/ali program, ki se financira iz sredstev skupnosti";
        String sectionV1Text = sectionV1.text();
        String isEuFundOriginal;
        if (sectionV1Text.contains(sectionV1PossibleTitle)) {
            sectionV1Text = sectionV1Text.substring(sectionV1Text.indexOf(sectionV1PossibleTitle) +
                    sectionV1PossibleTitle.length());
            if (sectionV1Text.isEmpty()) {
                // e.g. https://www.enarocanje.si/Obrazci/?id_obrazec=127334
                return null;
            } else {
                // e.g. https://www.enarocanje.si/Obrazci/?id_obrazec=127211 (just the information) or
                // https://www.enarocanje.si/Obrazci/?id_obrazec=127179 (more information)
                assert sectionV1Text.contains(".");
                isEuFundOriginal = sectionV1Text.split("\\p{Punct}")[0];
            }
        } else {
            // e.g. https://www.enarocanje.si/Obrazci/?id_obrazec=132520
            isEuFundOriginal = ParserUtils.getFromContent(sectionV1, null, " " +
                    ENarocanjeTenderFormUtils.IS_EU_FUND_TITLE);
            assert isEuFundOriginal.endsWith(".");
            isEuFundOriginal = isEuFundOriginal.substring(0, isEuFundOriginal.length() - 1);
        }

        return Collections.singletonList(new ParsedFunding()
                .setIsEuFund(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(isEuFundOriginal))));
    }

}
