package eu.datlab.worker.ro.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.slf4j.Logger;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Parser handler for RO source using ANNOTATION_call_2017, ANNOTATION_call_2019,
 * ANNOTATION_invitation_2016 and ANNOTATION_invitation_2018.
 */
public final class APATenderCsvCFTHandler {
    private static final String FORM_TYPE = "tipanunt"; // only in call_2019
    private static final String BUYER_ASSIGNED_ID = "numaranunt";
    private static final String PUBLICATION_DATE = "datapublicare";
    private static final String BUYER_NAME = "denumireautoritatecontractanta";
    private static final String BUYER_ID = "cui";
    private static final String BUYER_COUNTRY = "judet";
    private static final String SUPPLY_TYPE = "tipcontract";
    private static final String UTILITIES = "utilitati";
    private static final String PROCEDURE_TYPE = "tipprocedura";
    private static final String SELECTION_METHOD = "criteriuatribuire";
    private static final String ESTIMATED_PRICE_NET_AMOUNT = "valoareestimata";
    private static final String ESTIMATED_PRICE_CURRENCY = "moneda";
    private static final String MODALITATE = "modalitatedesfasurare";
    private static final String PUBLICATION_IN_TED = "trimislaojeu";
    private static final String IS_EU_FUND = "fonduricomunitare";
    private static final String CPV = "codcpv";
    private static final String CPV_LABELS = "denumirecodcpv";

    private static final String[] CSV_OLD_HEADER = {BUYER_ASSIGNED_ID, PUBLICATION_DATE,
            BUYER_NAME, BUYER_ID, BUYER_COUNTRY, SUPPLY_TYPE, UTILITIES, PROCEDURE_TYPE, SELECTION_METHOD,
            ESTIMATED_PRICE_NET_AMOUNT, ESTIMATED_PRICE_CURRENCY, MODALITATE, PUBLICATION_IN_TED, IS_EU_FUND,
            CPV, CPV_LABELS}; // 16

    private static final String[] CSV_NEW_CALL_HEADER = {FORM_TYPE, BUYER_ASSIGNED_ID,
            PUBLICATION_DATE, BUYER_NAME, BUYER_ID, BUYER_COUNTRY, SUPPLY_TYPE, UTILITIES, PROCEDURE_TYPE,
            SELECTION_METHOD, ESTIMATED_PRICE_NET_AMOUNT, ESTIMATED_PRICE_CURRENCY, MODALITATE, PUBLICATION_IN_TED,
            IS_EU_FUND, CPV, CPV_LABELS}; // 17

    private static final CSVFormat CSV_OLD_FORMAT = CSVFormat.newFormat('^').
            withHeader(CSV_OLD_HEADER).withIgnoreEmptyLines(true);
    private static final CSVFormat CSV_NEW_CALL_FORMAT = CSVFormat.newFormat('^').
            withHeader(CSV_NEW_CALL_HEADER).withIgnoreEmptyLines(true);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private APATenderCsvCFTHandler() {
    }

    /**
     * Parses given raw data.
     * @param raw raw data
     * @param logger logger
     * @return parsed tenders
     */
    public static List<ParsedTender> parse(final RawData raw, final Logger logger) {
        try {
            final List<ParsedTender> parsedTenders = new ArrayList<>();

            APACSVReader reader;
            boolean isNew = false;
            if (raw.getSourceData().split("\n", 2)[0].contains("TIP_ANUNT")) {
                reader = new APACSVReader(raw.getSourceData(), CSV_NEW_CALL_HEADER.length);
                isNew = true;
            } else {
                reader = new APACSVReader(raw.getSourceData(), CSV_OLD_HEADER.length);
            }
            reader.readLine(); // skip header

            String line;
            while ((line = reader.readLine()) != null) {
                if(line.isEmpty()) {
                    continue;
                }
                CSVParser csvParser;
                if (isNew) {
                    csvParser = CSVParser.parse(line, CSV_NEW_CALL_FORMAT);
                } else {
                    csvParser = CSVParser.parse(line, CSV_OLD_FORMAT);
                }

                for (CSVRecord tender : csvParser) {

                    ParsedBody buyer = new ParsedBody()
                            .setName(tender.get(BUYER_NAME))
                            .addBodyId(new BodyIdentifier()
                                    .setId(tender.get(BUYER_ID))
                                    .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                    .setScope(BodyIdentifier.Scope.RO))
                            .setAddress(new ParsedAddress()
                                    .setCountry(tender.get(BUYER_COUNTRY)));

                    parsedTenders.add(new ParsedTender()
                            .setEstimatedPrice(new ParsedPrice()
                                    .setNetAmount(tender.get(ESTIMATED_PRICE_NET_AMOUNT))
                                    .setCurrency(tender.get(ESTIMATED_PRICE_CURRENCY)))
                            .setNationalProcedureType(tender.get(PROCEDURE_TYPE))
                            .setProcedureType(tender.get(PROCEDURE_TYPE))
                            .setSelectionMethod(tender.get(SELECTION_METHOD))
                            .addBuyer(buyer)
                            .setBuyerAssignedId(tender.get(BUYER_ASSIGNED_ID))
                            .setSupplyType(tender.get(SUPPLY_TYPE))
                            .addPublication(new ParsedPublication()
                                    .setBuyerAssignedId(tender.get(BUYER_ASSIGNED_ID))
                                    .setPublicationDate(tender.get(PUBLICATION_DATE))
                                    .setSourceFormType(isNew ? tender.get(FORM_TYPE) : "CONTRACT_NOTICE")
                                    .setIsIncluded(true)
                                    .setSource(PublicationSources.RO_APA))
                            .addFunding(new ParsedFunding()
                                    .setIsEuFund(tender.get(IS_EU_FUND)))
                            .addCpv(new ParsedCPV()
                                    .setIsMain(String.valueOf(true))
                                    .setCode(tender.get(CPV))));
                }
            }
            return parsedTenders;
        } catch (IOException e) {
            logger.error("Parsing failed with exception {}", e);
            throw new UnrecoverableException("Unable to parse page", e);
        }
    }
}
