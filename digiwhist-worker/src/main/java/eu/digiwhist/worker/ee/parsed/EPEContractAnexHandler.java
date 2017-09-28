package eu.digiwhist.worker.ee.parsed;

import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedPayment;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;

/**
 * Contract anex handler for E-procurement in Estonia.
 *
 * @author Tomas Mrazek
 */
public final class EPEContractAnexHandler {

    /**
     * Supress default constructor for noninstantiability.
     */
    private EPEContractAnexHandler() {
        throw new AssertionError();
    }

    /**
     * Parses contract notice specific data.
     *
     * @param doc
     *      parsed document
     * @return parsed tender
     */
    public static ParsedTender parse(final Document doc) {
        Element context = EPEParserUtils.getDataTable(doc);
        Element partE = EPEParserUtils.parseFormPart("E LISA", context);
        Element partF = EPEParserUtils.parseFormPart("F LISA", context);

        return EPEParserUtils.parsePublicationAndTitle(doc)
            .addLot(new ParsedTenderLot()
            .setLotNumber(EPEParserUtils.regexValueByLabel("LEPING", "(?<value>\\d)", partE))
            .setTitle(EPEParserUtils.regexValueByLabel("Nimetus", "Nimetus (?<value>.+)", partE))
            .setEstimatedPrice(parseInlinePrice("Raamlepingu esialgne eeldatav maksumus", partE))
            .addBid(parseBid(partE, partF))
            .setEstimatedStartDate(EPEParserUtils.parseDateTime("Alguskuupäev", partE))
            .setCompletionDate(EPEParserUtils.parseDateTime("Lõppkuupäev", partE)));
    }

    /**
     * @param partE
     *      part E node (E LISA: Hankelepingu muudatused)
     * @param partF
     *      part F node (F LISA: Hankelepinguga kaasnevad allhanked)
     * @return return bid or null
     */
    private static ParsedBid parseBid(final Element partE, final Element partF) {
        List<Element> paymentsNodes = EPEParserUtils.parseRepeatedParts(
            JsoupUtils.selectFirst("tr:contains(TEAVE RAAMLEPINGU ALUSEL SÕLMITUD HANKELEPINGUTE KOHTA) + tr", partE),
            null, "Jrk\\.nr\\.");

        List<ParsedPayment> payments = null;
        if (paymentsNodes != null) {
            payments = paymentsNodes.stream()
                .map(n -> { 
                        return new ParsedPayment().setPrice(parseInlinePrice("Maksumus", n)); 
                    })
                .collect(Collectors.toList());
        } else {
            ParsedPrice price = parseInlinePrice("Lepingu täitmise tegelik maksumus", partE);
            if (price != null) {
                payments = Collections.singletonList(new ParsedPayment().setPrice(price));
            }
        }

        // TODO
        // Jrk.nr. - lot number ???
        // Kirjeldus - lot description ???

        return new ParsedBid()
            .setIsWinning(Boolean.TRUE.toString())
            .setPrice(parseInlinePrice("Lepingu maksumus sõlmimise hetkel", partE))
            .setPayments(payments)
            .setSubcontractedProportion(EPEParserUtils.regexValueByLabel("Hankelepinguga kaasneva allhanke osakaal",
                "(?<value>[\\d\\.,])", partF))
            .setSubcontractors(Collections.singletonList(EPEParserUtils.parseBody("KASUTATUD ALLHANKIJA NIMI JA"
                + " AADRESS, KELLEGA HANKELEPING SÕLMITI", partF)))
            .setSubcontractedValue(EPEParserUtils.parsePrice("Andmed allhankelepingu maksumuse kohta", partF));
    }

    /**
     * Parses price which is listed in one row.
     *
     * @param label
     *      label
     * @param context
     *      context that includes inlune price data
     * @return price or null
     */
    private static ParsedPrice parseInlinePrice(final String label, final Element context) {
        Element node = JsoupUtils.selectFirst("tr:matches(" + label+ ")", context);
        if (node == null) {
            return null;
        }

        Matcher m = Pattern.compile(".+(?<amount>[\\d,]+) Rahaühik: (?<currency>[^ ]+)").matcher(node.text());
        if (m.find()) {
            return new ParsedPrice()
                .setNetAmount(m.group("amount"))
                .setCurrency(EPEParserUtils.parseCurrency(m.group("currency")));
        }
        
        return null;
    }
}
