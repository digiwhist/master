package eu.datlab.worker.pl.parsed;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;

import java.io.IOException;
import java.util.List;

import static eu.datlab.worker.pl.parsed.UZPJsonTenderUtils.parseItems;
import static eu.datlab.worker.pl.parsed.UZPJsonTenderUtils.textValue;


/**
 * Handler for Json notices.
 */
public final class UZPJsonTenderAwardHandler {

    /**
     * Private constructor for noninstatiability.
     */
    private UZPJsonTenderAwardHandler() {
    }

    /**
     * Parse award data.
     *
     * @param biddersString bidders
     * @param awardInformationString award information
     * @param parsedTender parsedTender
     */
    public static void parse(final String biddersString, final String awardInformationString, final ParsedTender parsedTender) {
        ObjectMapper mapper = new ObjectMapper();
        final JsonNode biddersJson;
        final JsonNode awardJson;
        try {
            biddersJson = mapper.readTree(biddersString.replaceAll("<.*>", ""));
            awardJson = mapper.readTree(awardInformationString.replaceAll("<.*>", ""));
        } catch (IOException e) {
            throw new UnrecoverableException("Unable to parse JSON from raw data", e);
        }

        List<JsonNode> lots = parseItems(awardJson.findValue("ZP_403_Zal"));
        List<JsonNode> bidders = parseItems(biddersJson.findValue("ZP_403_Wykonawcy"));


        int positionOnPage = 1;
        for (JsonNode lot : lots) {
            final String lotNumber = textValue("ZamowienieCzescNr", lot);

            JsonNode bidder = bidders.stream()
                    .filter(b -> textValue("ZamowienieCzescNr", b) != null)
                    .filter(b -> textValue("ZamowienieCzescNr", b).equals(lotNumber))
                    .findFirst()
                    .orElse(null);

            ParsedBody parsedBidder = null;
            if (bidder != null) {
                parsedBidder = new ParsedBody()
                        .setName(textValue("Nazwa", bidder))
                        .setAddress(new ParsedAddress()
                                .setStreet(textValue("AdresPocztowy", bidder))
                                .setCity(textValue("Miejscowosc", bidder))
                        )
                        .setEmail(textValue("Email", bidder));
            }

            parsedTender.addLot(new ParsedTenderLot()
                    .setTitle(textValue("ZamowienieNazwa", lot))
                    .setLotNumber(lotNumber)
                    .setPositionOnPage(String.valueOf(positionOnPage))
                    .setAwardDecisionDate(textValue("ZamowienieDataUdzieleniaZamowienia", lot))
                    .setBidsCount(textValue("LiczbaOtrzymanychOfert", lot))
                    .setElectronicBidsCount(textValue("LiczbaOtrzymanychOfertElektronicznych", lot))
                    .setBidsCount(textValue("LiczbaOtrzymanychOfertElektronicznych", lot))
                    .setEstimatedPrice(new ParsedPrice()
                            .setNetAmount(textValue("ZamowienieSzcunkowaWartoscZamowieniaCalkowita", lot))
                            .setCurrency(textValue("WalutaZamowienia", lot))
                    )
                    .addBid(new ParsedBid()
                            .setPrice(new ParsedPrice()
                                    .setNetAmount(textValue("CenaWybranejOferty", lot))
                                    .setCurrency(textValue("InnaWaluta", lot))
                            )
                            .addBidder(parsedBidder)
                    )
            );

            positionOnPage++;
        }


    }
}
