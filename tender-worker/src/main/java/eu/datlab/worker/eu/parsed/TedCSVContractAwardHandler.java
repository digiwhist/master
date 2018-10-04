package eu.datlab.worker.eu.parsed;

import java.util.Arrays;

import org.apache.commons.csv.CSVRecord;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;

/**
 * The handler for TED contract notice CVS record.
 *
 * @author Tomas Mrazek
 */
public final class TedCSVContractAwardHandler {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedCSVContractAwardHandler() {
        throw new AssertionError();
    }

    /**
     * Parses contract notice CSV record specific data.
     *
     * @param parsedTender
     *      parsed tender with common data
     * @param record
     *      CSV record
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final CSVRecord record) {

        parsedTender
            .setFinalPrice(TedCSVTenderParserUtils.parseTenderPrice(record))
            .setProcedureType(record.get("top_type"))
            .addLot(new ParsedTenderLot()
                .addBid(new ParsedBid()
                    .setIsWinning(Boolean.TRUE.toString())
                    .addBidder(parseWinner(record))
                    .setPrice(TedCSVTenderParserUtils.parsePrice(record, Arrays.asList("award_value_euro_fin_1",
                        "award_value_euro")))
                    .setIsSubcontracted(TedCSVTenderParserUtils.parseBoolean(record, "b_subcontracted")))
                // record.get("ID_AWARD") maybe also contract number
                .setContractNumber(record.get("contract_number"))
                .setLotNumber(record.get("lot_number"))
                .setTitle(record.get("title"))
                .setBidsCount(record.get("number_offers"))
                .setElectronicBidsCount("number_offers_electr"))
                .setEstimatedPrice(TedCSVTenderParserUtils.parsePrice(record, Arrays.asList("award_est_value_euro")))
                .setAwardDecisionDate(record.get("dt_award"));

        return parsedTender;
    }

    /**
     * Parses winner from the CSV record.
     *
     * @param record
     *      CSV record
     * @return winner
     */
    private static ParsedBody parseWinner(final CSVRecord record) {
        return new ParsedBody()
            .setName(record.get("win_name"))
            .setAddress(new ParsedAddress()
                .setStreet(record.get("win_address"))
                .setCity(record.get("win_town"))
                .setPostcode(record.get("win_postal_code"))
                .setCountry(record.get("win_country_code")));
    }
}
