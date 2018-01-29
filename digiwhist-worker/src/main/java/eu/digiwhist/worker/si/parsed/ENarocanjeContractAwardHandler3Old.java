package eu.digiwhist.worker.si.parsed;

import static eu.digiwhist.worker.si.parsed.BaseENarocanjeFormInTableHandler.parsePrice;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.StringUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import java.util.Arrays;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.TextNode;

/**
 * Handler for parsing old forms "EU 6 - SL" and "NMV2".
 */
final class ENarocanjeContractAwardHandler3Old extends BaseENarocanjeContractAwardInTableHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractAwardHandler3Old() {}

    /**
     * Parses form specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from form
     */
    public static ParsedTender parse(final ParsedTender tender, final Element form) {
        parseCommonContractAwardAttributes(tender, form);

        // supply type
        Element node = JsoupUtils.selectFirst("div.tab-content > center > font.naslov", form);
        if (node != null && node.childNodeSize() > 0) {
            tender.setSupplyType(
                // find first text node, its value is supplyType
                node.childNodes().stream()
                    .filter(n -> n instanceof TextNode)
                    .map(n -> ((TextNode) n).text())
                    .findFirst().orElse(null));
        }

        tender.setDescription(ENarocanjeTenderFormInTableUtils.getSectionContent("II.1.4", form))
            .setCpvs(parseTenderCpvs("II.1.5", form))
            .setIsCoveredByGpa(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                ENarocanjeTenderFormInTableUtils.getSectionContent("II.1.6", form, Arrays.asList(
                    "Naročilo je vključeno v Sporazum o vladnih naročilih (GPA)")))))
            .setFinalPrice(parsePrice(ParserUtils.getFromContent(
                ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("II.2.1", form), null, " Vrednost:")))
            .setIsElectronicAuction(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                ENarocanjeTenderFormInTableUtils.getSectionContent("IV.2.2", form, Arrays.asList(
                    "Uporabljena je bila elektronska dražba")))))
            .setSelectionMethod(StringUtils.removeDotsAtTheEnd(ParserUtils.getFromContent(
                ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("IV.2.1", form), null, 6)));

        // procedure and nationalProceduType
        Element sectionIV11 = ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("IV.1.1", form);
        if (sectionIV11 != null && sectionIV11.childNodeSize() >= 12) {
            String procedureType = sectionIV11.childNode(12).toString();

            tender.setNationalProcedureType(procedureType).setProcedureType(procedureType);
        }

        // lots
        Element priceNode = ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("^V.1.4\\)", form);

        tender.addLot(new ParsedTenderLot()
            .setContractNumber(ENarocanjeTenderFormInTableUtils.getSectionContent("ŠT. NAROČILA:", form, Arrays.asList(
                "ŠT. NAROČILA:")))
            .setAwardDecisionDate(ENarocanjeTenderFormInTableUtils.getSectionContent("^V.1.1\\)", form, Arrays.asList(
                "Datum oddaje naročila:")))
            .setBidsCount(ENarocanjeTenderFormInTableUtils.getSectionContent("^V.1.2\\)", form, Arrays.asList(
                "Število prejetih ponudb")))
            .setEstimatedPrice(parsePrice(ParserUtils.getFromContent(priceNode, null, " Začetna skupna ocenjena"
                + " vrednost naročila:")))
            .addBid(new ParsedBid()
                .setIsSubcontracted(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                    ENarocanjeTenderFormInTableUtils.getSectionContent("V.1.5", form, Arrays.asList(
                        "Naročilo bo verjetno s pogodbo oddano podizvajalcem")))))
                .addBidder(ENarocanjeTenderFormUtils.parseNameAndAddressBody(
                    ENarocanjeTenderFormInTableUtils.getSectionContent("^V.1.3\\)", form, Arrays.asList(
                        "Ime in naslov gospodarskega subjekta, ki mu je bilo naročilo oddano:")), null))
                .setPrice(parsePrice(ParserUtils.getFromContent(priceNode, null, " Vrednost:")))));


        return tender;
    }

}
