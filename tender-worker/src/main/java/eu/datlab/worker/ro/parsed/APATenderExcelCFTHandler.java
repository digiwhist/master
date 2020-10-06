package eu.datlab.worker.ro.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.slf4j.Logger;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Parser handler for RO source using ANNOTATION_call_2017, ANNOTATION_call_2019,
 * ANNOTATION_invitation_2016 and ANNOTATION_invitation_2018.
 */
public final class APATenderExcelCFTHandler {
    private static final String A = "nr.crt.";
    private static final String BUYER_ASSIGNED_ID = "numaranunt"; // "Numar Anunt" or "umarAnunt" or "NUMAR_ANUNT" -> "numaranunt"
    private static final String BUYER_ASSIGNED_ID_SECOND_FORM = "numaranuntinvitatie";
    private static final String BUYER_ASSIGNED_ID_THIRD_FORM = "numarinvitatie";
    private static final String PUBLICATION_DATE = "datapublicare";
    private static final String BUYER_NAME = "denumireautoritatecontractanta";
    private static final String BUYER_NAME_SECOND_FORM = "denumireac";
    private static final String BUYER_ID = "cui";
    private static final String BUYER_ID_SECOND_FORM = "cuiac";
    private static final String BUYER_COUNTRY = "judet";
    private static final String SUPPLY_TYPE = "tipcontract";
    private static final String UTILITIES = "utilitati";
    private static final String PROCEDURE_TYPE = "tipprocedura";
    private static final String SELECTION_METHOD = "criteriuatribuire";
    private static final String ESTIMATED_PRICE_NET_AMOUNT = "valoareestimata";
    private static final String ESTIMATED_PRICE_CURRENCY = "moneda";
    private static final String L = "modalitatedesfasurare";
    private static final String PUBLICATION_IN_TED = "trimislaojeu";
    private static final String PUBLICATION_IN_TED_SECOND_FORM = "trimisojeu";
    private static final String IS_EU_FUND = "fonduricomunitare";
    private static final String CPV = "codcpv";
    private static final String CPV_SECOND_FORM = "maincpvcode";
    private static final String CPV_THIRD_FORM = "maincpv";
    private static final String CPV_LABELS = "denumirecodcpv";
    private static final String CPV_LABELS_SECOND_FORM = "maincpvname";
    private static final String FORM_TYPE = "tipanunt";

    /**
     * Suppress default constructor for noninstantiability.
     */
    private APATenderExcelCFTHandler(){

    }

    /**
     * Parses the given sheet.
     *
     * @param sheet  sheet with raw data
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

            String buyerName = checkingValue(headerIndexes.get(BUYER_NAME), row);
            if (buyerName == null) {
                buyerName = checkingValue(headerIndexes.get(BUYER_NAME_SECOND_FORM), row);
            }
            String buyerId = checkingValue(headerIndexes.get(BUYER_ID), row);
            if (buyerId == null) {
                buyerId = checkingValue(headerIndexes.get(BUYER_ID_SECOND_FORM), row);
            }
            String buyerCountry = checkingValue(headerIndexes.get(BUYER_COUNTRY), row);
            String publicationDate = checkingValue(headerIndexes.get(PUBLICATION_DATE), row);
            String estimatedPriceNetAmount = checkingValue(headerIndexes.get(ESTIMATED_PRICE_NET_AMOUNT), row);
            String estimatedPriceCurrency = checkingValue(headerIndexes.get(ESTIMATED_PRICE_CURRENCY), row);
            String procedureType = checkingValue(headerIndexes.get(PROCEDURE_TYPE), row);
            String selectionMethod = checkingValue(headerIndexes.get(SELECTION_METHOD), row);
            String buyerAssignedId = checkingValue(headerIndexes.get(BUYER_ASSIGNED_ID), row);
            if (buyerAssignedId == null) {
                buyerAssignedId = checkingValue(headerIndexes.get(BUYER_ASSIGNED_ID_SECOND_FORM), row);
                if (buyerAssignedId == null) {
                    buyerAssignedId = checkingValue(headerIndexes.get(BUYER_ASSIGNED_ID_THIRD_FORM), row);
                }
            }
            String supplyType = checkingValue(headerIndexes.get(SUPPLY_TYPE), row);
            String isEuFund = checkingValue(headerIndexes.get(IS_EU_FUND), row);
            String cpvCode = checkingValue(headerIndexes.get(CPV), row);
            if (cpvCode == null) {
                cpvCode = checkingValue(headerIndexes.get(CPV_SECOND_FORM), row);
                if (cpvCode == null) {
                    cpvCode = checkingValue(headerIndexes.get(CPV_THIRD_FORM), row);
                }
            }
            String formType = checkingValue(headerIndexes.get(FORM_TYPE), row);
            if (formType == null) { // ANNOTATION_call_2019
                formType = String.valueOf(PublicationFormType.CONTRACT_NOTICE);
            }

            ParsedBody buyer = new ParsedBody()
                    .setName(buyerName)
                    .addBodyId(new BodyIdentifier()
                            .setId(buyerId)
                            .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                            .setScope(BodyIdentifier.Scope.RO))
                    .setAddress(new ParsedAddress()
                            .setCountry(buyerCountry));

            parsedTenders.add(new ParsedTender()
                    .setEstimatedPrice(new ParsedPrice()
                            .setNetAmount(estimatedPriceNetAmount)
                            .setCurrency(estimatedPriceCurrency))
                    .setNationalProcedureType(procedureType)
                    .setSelectionMethod(selectionMethod)
                    .addBuyer(buyer)
                    .setBuyerAssignedId(buyerAssignedId)
                    .setSupplyType(supplyType)
                    .addPublication(new ParsedPublication()
                            .setBuyerAssignedId(buyerAssignedId)
                            .setPublicationDate(publicationDate)
                            .setSourceFormType(formType)
                            .setIsIncluded(true)
                            .setSource(PublicationSources.RO_APA))
                    .addFunding(new ParsedFunding()
                            .setIsEuFund(isEuFund))
                    .addCpv(new ParsedCPV()
                            .setIsMain(String.valueOf(true))
                            .setCode(cpvCode)));
        }
        return parsedTenders;
    }

    /**
     * Checks if the value is not null and if not returns the value frm the cell with this index.
     *
     * @param valueToBeChecked index of cell if not null
     * @param row              row with cells
     * @return value from the cell or null if the index is null
     */
    private static String checkingValue(final Integer valueToBeChecked, final Row row) {
        if (valueToBeChecked != null) {
            return APATenderExcelHandler.getCellValue(row.getCell(valueToBeChecked));
        }
        return null;
    }

}
