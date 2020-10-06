package eu.datlab.worker.sk.parsed;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.List;

import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getFirstValueFromElement;
import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.parsePrice;

/**
 * Handler for summary reports.
 */
public class UvoTenderSummaryReportsHandler {


    /**
     * Parses summary reports.
     * @param document document to parse
     * @param basePublication common publication
     * @param baseBuyer common buyer
     * @param baseSupplyType common suply type
     * @return parsed tenders
     */
    public final List<ParsedTender> parse(final Document document, final ParsedPublication basePublication,
                             final ParsedBody baseBuyer, final String baseSupplyType) {
        List<ParsedTender> parsedTenders = new ArrayList<>();
        Element sectionV = document.selectFirst("legend:containsOwn(ODDIEL V: ZADANIE ZÁKAZKY)").parent();
        Elements partHeaders = sectionV.select("span:containsOwn(Časť:), span:containsOwn(Časť č.:)");
        // if only one part, there is no header Časť:
        if (partHeaders.isEmpty()) {
            parsedTenders.add(parseTender(sectionV));
        }
        int tendersCount = partHeaders.size();
        // parse every tender
        for (int i = 0; i < tendersCount; i++) {
            Element tenderElem = null;
            if (i == (tendersCount - 1)) {
                // lust subsection of elements - from the last header Časť: to the end of the part V
                tenderElem = ParserUtils.getSubsectionOfElements(partHeaders.get(i),
                        partHeaders.get(i).lastElementSibling()).appendChild(partHeaders.get(i).lastElementSibling());
            } else {
                // subsection of elements between two headers Časť:
                tenderElem = ParserUtils.getSubsectionOfElements(partHeaders.get(i), partHeaders.get(i + 1));
            }
            parsedTenders.add(parseTender(tenderElem));
        }
        // for every parsed tender set common buyer, publication and supply type
        for (ParsedTender parsedTender : parsedTenders) {
            parsedTender
                    .addPublication(basePublication)
                    .addBuyer(baseBuyer);
            if (parsedTender.getSupplyType() == null || parsedTender.getSupplyType().isEmpty()) {
                parsedTender.setSupplyType(baseSupplyType);
            }
        }
        return parsedTenders;
    }


    /**
     * Parses one tender.
     * @param tenderElement element to parse
     * @return parsed tender
     */
    private ParsedTender parseTender(final Element tenderElement) {
        String sourceUrl = UvoTenderParserUtils.getFirstOwnValueFromElement(tenderElement,
                "div:containsOwn(Odkaz na zverejnenú zmluvu:) > span");
        ParsedTender tender =
                new ParsedTender()
                        .setTitle(UvoTenderParserUtils.getFirstOwnValueFromElement(tenderElement,
                                "span:containsOwn(Názov:) + span"))
                        .addPublication(new ParsedPublication()
                                .setHumanReadableUrl(sourceUrl)
                                .setSource(parseSource(sourceUrl))
                                .setSourceFormType(PublicationFormType.CONTRACT_AWARD.toString())
                                .setIsIncluded(false))
                        .setSupplyType(UvoTenderParserUtils.getFirstValueFromElement(tenderElement,
                                "span:containsOwn(Druh zákazky:) + span"))
                        .setContractSignatureDate(getFirstValueFromElement(tenderElement, "div:has(span:containsOwn(Dátum " +
                                "uzatvorenia zmluvy)) + div"))
                        .addLot(new ParsedTenderLot()
                                .setBidsCount(UvoTenderParserUtils.getFirstOwnValueFromElement(tenderElement,
                                        "div:containsOwn(Počet prijatých ponúk:) > span"))
                                .setEnvisagedCandidatesCount(UvoTenderParserUtils.getFirstOwnValueFromElement(tenderElement,
                                        "div:containsOwn(Počet oslovených uchádzačov:) > span"))
                                .addBid(new ParsedBid()
                                        .setIsWinning(Boolean.TRUE.toString())
                                        .setBidders(UvoTenderParserUtils.parseBidders(tenderElement))
                                .setPrice(parseEstimatedPrice(tenderElement))));
        return tender;
    }

    /**
     * Parses source from source url.
     *
     * @param sourceUrl source url
     * @return parsed source
     */
    private String parseSource(final String sourceUrl) {
        if (sourceUrl == null) {
            return null;
        }
        String[] parts = sourceUrl.split(".sk");
        if (parts.length > 0) {
            return parts[0] + ".sk";
        } else {
            return null;
        }
    }


    /**
     * Parse estimated price.
     *
     * @param lot lot to be parsed from
     * @return ParsedPrice or null
     */
    private static ParsedPrice parseEstimatedPrice(final Element lot) {
        final String[] priceSelectors = new String[]{
                "div:has(span:containsOwn(Informácie o hodnote zákazky/časti)) + div > span"
        };

        final String[] currencySelectors = new String[]{
                "div:has(span:containsOwn(Informácie o hodnote zákazky/časti)) + div > span + span"
        };

        return parsePrice(lot, false, priceSelectors, currencySelectors, null);
    }

}
