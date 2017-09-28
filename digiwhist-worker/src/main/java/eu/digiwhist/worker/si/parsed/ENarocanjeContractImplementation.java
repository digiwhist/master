package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Handler for parsing forms "OS - ZJN-2" and "OS - ZJNVETPS".
 */
final class ENarocanjeContractImplementation extends BaseENarocanjeFormInTableHandler {
    private static final Logger logger = LoggerFactory.getLogger(ENarocanjeContractImplementation.class);

    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractImplementation() {}

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

        final Element sectionIII13 = getSectionWithSeparatedNodes("III.1.3", form);
        final Element preamble = JsoupUtils.selectFirst(
                "div.tab-content > table > tbody > tr:contains(Vrsta postopka:)", form);

        final String estimatedPriceTitle = " Začetna ocenjena vrednost sklopa brez DDV:";
        final String finalPriceTitle = " Skupna končna vrednost sklopa brez DDV:";

        final String sectionIII13Text = sectionIII13.text();
        if (!sectionIII13Text.contains(estimatedPriceTitle) && !sectionIII13Text.contains(finalPriceTitle)) {
            logger.warn("Neither estimated price nor final price is in publication");
        }

        tender
                .setEstimatedPrice(parsePrice(ParserUtils.getFromContent(sectionIII13, null, estimatedPriceTitle)))
                .setFinalPrice(parsePrice(ParserUtils.getFromContent(sectionIII13, null, finalPriceTitle)))
                .setNationalProcedureType(ParserUtils.getFromContent(preamble, null, " Vrsta postopka:"))
                .setLots(parseLots(form));

        return tender;
    }

    /**
     * Parses all the lots from document.
     *
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return list of all parsed lots or null if no lots specified
     */
    private static List<ParsedTenderLot> parseLots(final Element form) {
        final Elements lotTitleElements = getSections("ODDELEK III: PODATKI O ODDANEM SKLOPU OZ. NAROČILU", form);
        if (lotTitleElements.isEmpty()) {
            return null;
        }

        // get elements representing lots
        final List<Element> lotElements = new ArrayList<>();
        final Element sectionIVElement = getSection("ODDELEK IV: PODATKI O OBJAVI", form);
        for (int i = 0; i < lotTitleElements.size() - 1; ++i) {
            lotElements.add(ParserUtils.getSubsectionOfElements(lotTitleElements.get(i), lotTitleElements.get(i + 1)));
        }
        lotElements.add(ParserUtils.getSubsectionOfElements(lotTitleElements.get(lotTitleElements.size() - 1),
                sectionIVElement));

        List<ParsedTenderLot> lots = new ArrayList<>();

        for (Element lotElement : lotElements) {
            final Element sectionIII211 = ParserUtils.getSubsectionOfNodes(
                    JsoupUtils.selectFirst("tr:contains(III.2)", lotElement),
                    JsoupUtils.selectFirst("tr:contains(ODDELEK IV:)", lotElement));

            // get string where bidder name and address is
            final String infoSeparator = "   ";
            // row index (2/4/8/16) is checked from observation (it is the first row in the section)
            String nameAndAddress = ParserUtils.getFromContent(sectionIII211, null, 2);
            int infoSeparatorIndex1 = nameAndAddress.indexOf(infoSeparator);
            int infoSeparatorIndex2 = nameAndAddress.indexOf(infoSeparator, infoSeparatorIndex1
                    + infoSeparator.length());
            if (!(nameAndAddress.contains(infoSeparator) && infoSeparatorIndex1 < infoSeparatorIndex2)) {
                nameAndAddress = ParserUtils.getFromContent(sectionIII211, null, 4);
                infoSeparatorIndex1 = nameAndAddress.indexOf(infoSeparator);
                infoSeparatorIndex2 = nameAndAddress.indexOf(infoSeparator, infoSeparatorIndex1
                        + infoSeparator.length());
            }
            if (!(nameAndAddress.contains(infoSeparator) && infoSeparatorIndex1 < infoSeparatorIndex2)) {
                nameAndAddress = ParserUtils.getFromContent(sectionIII211, null, 8);
                infoSeparatorIndex1 = nameAndAddress.indexOf(infoSeparator);
                infoSeparatorIndex2 = nameAndAddress.indexOf(infoSeparator, infoSeparatorIndex1
                        + infoSeparator.length());
            }
            if (!(nameAndAddress.contains(infoSeparator) && infoSeparatorIndex1 < infoSeparatorIndex2)) {
                if (sectionIII211.html().split("<br>").length < 17) {
                    // there is no name and address - e.g. 21. lot in
                    // https://www.enarocanje.si/Obrazci/?id_obrazec=148719
                    continue;
                }
                nameAndAddress = ParserUtils.getFromContent(sectionIII211, null, 16);
                infoSeparatorIndex1 = nameAndAddress.indexOf(infoSeparator);
                infoSeparatorIndex2 = nameAndAddress.indexOf(infoSeparator, infoSeparatorIndex1
                        + infoSeparator.length());
            }
            assert nameAndAddress.contains(infoSeparator) && infoSeparatorIndex1 < infoSeparatorIndex2;
            nameAndAddress = nameAndAddress.substring(infoSeparatorIndex1, infoSeparatorIndex2);

            // get bids count
            final Element bidsCountElement = JsoupUtils.selectFirst("tr:contains(III.1.1)", lotElement);
            final String textOfBidsCountElement = bidsCountElement.text();
            final String bidsCount = textOfBidsCountElement.substring(textOfBidsCountElement.indexOf(':') + 1);

            final ParsedTenderLot lot = new ParsedTenderLot()
                    .setBidsCount(bidsCount);
            parseNameAndAddressOfWinningBidder(nameAndAddress, lot);
            lots.add(lot);
        }

        return lots;
    }

}
