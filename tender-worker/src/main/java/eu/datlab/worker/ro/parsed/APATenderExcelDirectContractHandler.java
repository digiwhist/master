package eu.datlab.worker.ro.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;

import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.slf4j.Logger;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Handler for parsing excel using ANNOTATION_directcontract_2007 and ANNOTATION_directcontract_2019.
 */
public final class APATenderExcelDirectContractHandler {
    private static final String BIDDER_NAME = "castigator";  // Castigator in 2007 and CASTIGATOR in 2019
    private static final String BIDDER_IDS = "castigatorcui";
    private static final String BIDDER_COUNTRY = "castigatortara";
    private static final String BIDDER_CITY = "ccastigatorlocalitate";
    private static final String BIDDER_RAW_ADDRESS = "castigatoradresa";
    private static final String PROCEDURE_TYPE = "tipprocedura";
    private static final String BUYER_NAME = "autoritatecontractanta";
    private static final String BUYER_IDS = "autoritatecontractantacui";
    private static final String ASSIGNED_ID = "numaranunt";
    private static final String PUBLICATION_DATE = "dataanunt";
    private static final String TIP_INCHEIERE_CONTRACT = "tipincheierecontract";
    private static final String BUYER_ASSIGNED_ID = "numarcontract";
    private static final String CONTRACT_SIGNATURE_DATE = "datacontract";
    private static final String TITLE = "titlucontract";
    private static final String NET_AMOUNT = "valoare";
    private static final String CURRENCY = "moneda";
    private static final String NET_AMOUNT_NATIONAL = "valoareron";
    private static final String NET_AMOUNT_EUR = "valoareeur";
    private static final String L = "cpvcodeid";
    private static final String CPV = "cpvcode";
    private static final String DESCRIPTION = "descriere"; // only 2019

    /**
     * Suppress default constructor for noninstantiability.
     */
    private APATenderExcelDirectContractHandler(){

    }

    /**
     * Parses the given sheet.
     * @param sheet sheet with raw data
     * @param logger logger
     * @return list of parsed tenders
     */
    public static List<ParsedTender> parse(final Sheet sheet, final Logger logger) {

        final Row headerRow = sheet.getRow(0);
        HashMap<String, Integer> headerIndexes = new HashMap<>();
        for (int i = 0; i < headerRow.getLastCellNum(); i++) {
            headerIndexes.put(headerRow.getCell(i).getStringCellValue()
                    .replace(" ", "")
                    .replace("_", "")
                    .toLowerCase(), i);
        }
        final List<ParsedTender> parsedTenders = new ArrayList<>();
        Row row;
        for (int i = 1; i <= sheet.getLastRowNum(); i++) {
            row = sheet.getRow(i);

            String bidderName = checkingValue(headerIndexes.get(BIDDER_NAME), row);
            String bidderId = checkingValue(headerIndexes.get(BIDDER_IDS), row);
            String bidderCountry = checkingValue(headerIndexes.get(BIDDER_COUNTRY), row);
            String bidderCity = checkingValue(headerIndexes.get(BIDDER_CITY), row);
            String bidderRawAddress = checkingValue(headerIndexes.get(BIDDER_RAW_ADDRESS), row);
            String procedureType = checkingValue(headerIndexes.get(PROCEDURE_TYPE), row);
            String buyerName = checkingValue(headerIndexes.get(BUYER_NAME), row);
            String buyerIds = checkingValue(headerIndexes.get(BUYER_IDS), row);
            String assignedId = checkingValue(headerIndexes.get(ASSIGNED_ID), row);
            String publicationDate = checkingValue(headerIndexes.get(PUBLICATION_DATE), row);
            String buyerAssignedId = checkingValue(headerIndexes.get(BUYER_ASSIGNED_ID), row);
            String contractSignatureDate = checkingValue(headerIndexes.get(CONTRACT_SIGNATURE_DATE), row);
            String title = checkingValue(headerIndexes.get(TITLE), row);
            String netAmount = checkingValue(headerIndexes.get(NET_AMOUNT), row);
            String currency = checkingValue(headerIndexes.get(CURRENCY), row);
            String netAmountEur = checkingValue(headerIndexes.get(NET_AMOUNT_EUR), row);
            String cpvCode = checkingValue(headerIndexes.get(CPV), row);
            String description = checkingValue(headerIndexes.get(DESCRIPTION), row);

            parsedTenders.add(new ParsedTender()
                    .addLot(new ParsedTenderLot()
                            .setBidsCount(String.valueOf(1))
                            .addBid(new ParsedBid()
                                    .addBidder(new ParsedBody()
                                            .setName(bidderName)
                                            .addBodyId(new BodyIdentifier()
                                                    .setId(bidderId)
                                                    .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                                    .setScope(BodyIdentifier.Scope.RO))
                                            .setAddress(new ParsedAddress()
                                                    .setCountry(bidderCountry)
                                                    .setCity(bidderCity)
                                                    .setRawAddress(bidderRawAddress)))
                                    .setPrice(new ParsedPrice()
                                            .setNetAmount(netAmount)
                                            .setCurrency(currency)
                                            .setNetAmountEur(netAmountEur))
                                    .setIsWinning(String.valueOf(true)))
                            .setContractSignatureDate(contractSignatureDate))
                    .setNationalProcedureType(procedureType)
                    .setProcedureType(procedureType)
                    .addBuyer(new ParsedBody()
                            .setName(buyerName)
                            .addBodyId(new BodyIdentifier()
                                    .setId(buyerIds)
                                    .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                    .setScope(BodyIdentifier.Scope.RO)))
                    .setBuyerAssignedId(buyerAssignedId)
                    .addPublication(new ParsedPublication()
                            .setBuyerAssignedId(assignedId)
                            .setPublicationDate(publicationDate)
                            .setSourceFormType("CONTRACT_AWARD")
                            .setIsIncluded(true)
                            .setSource(PublicationSources.RO_APA))
                    .addCpv(new ParsedCPV()
                            .setIsMain(String.valueOf(true))
                            .setCode(cpvCode))
                    .setTitle(title)
                    .setDescription(description));
        }
        return parsedTenders;
    }

    /**
     * Checks if the value is not null and if not returns the value frm the cell with this index.
     * @param valueToBeChecked index of cell if not null
     * @param row row with cells
     * @return value from the cell or null if the index is null
     */
    private static String checkingValue(final Integer valueToBeChecked, final Row row) {
        if (valueToBeChecked != null) {
            return APATenderExcelHandler.getCellValue(row.getCell(valueToBeChecked));
        }
        return null;
    }
}

