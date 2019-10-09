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
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.slf4j.Logger;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;


/**
 * Parser handler for RO source using ANNOTATION_contracte_2007 and ANNOTATION_contracte_2019.
 */
public final class APATenderExcelContracteHandler {
    private static final String BIDDER_NAME = "castigator";
    private static final String BIDDER_IDS = "castigatorcui";
    private static final String BIDDER_COUNTRY = "castigatortara";
    private static final String BIDDER_CITY = "castigatorlocalitate";
    private static final String BIDDER_RAW_ADDRESS = "castigatoradresa";
    private static final String FORM_TYPE = "tip"; // not used
    private static final String FORM_TYPE_SECOND_FORM = "tipanunt";
    private static final String SUPPLY_TYPE = "tipcontract";
    private static final String PROCEDURE_TYPE = "tipprocedura";
    private static final String BUYER_NAME = "autoritatecontractanta";
    private static final String BUYER_IDS = "autoritatecontractantacui";
    private static final String BUYER_TYPE = "tipac";
    private static final String BUYER_MAIN_ACTIVITIES = "tipactivitateac";
    private static final String AWARD_PUBLICATION_BUYER_ASSIGNED_ID = "numaranuntatribuire";
    private static final String AWARD_PUBLICATION_DATE = "dataanuntatribuire";
    private static final String IS_FRAMEWORK_AGREEMENT = "tipincheierecontract";
    private static final String SELECTION_METHOD = "tipcriteriiatribuire";
    private static final String IS_ELECTRONIC_AUCTION = "cuLicitatieelectronica";
    private static final String BIDS_COUNT = "numaroferteprimite";
    private static final String BIDS_COUNT_SECOND_FORM = "numaroferte";
    private static final String IS_SUBCONTRACTATED = "subcontractat";
    private static final String LOT_BUYER_ASSIGNED_ID = "numarcontract";
    private static final String CONTRACT_SIGNATURE_DATE = "datacontract";
    private static final String TITLE = "titlucontract";
    private static final String BID_PRICE_NET_AMOUNT = "valoare";
    private static final String BID_PRICE_NET_AMOUNT_SECOND_FORM = "valoarecontract";
    private static final String BID_PRICE_CURRENCY = "moneda";
    private static final String BID_PRICE_NET_AMOUNT_NATIONAL = "valoareron";  // not used
    private static final String BID_PRICE_NET_AMOUNT_NATIONAL_SECOND_FORM = "valoarecontractron";
    private static final String BID_PRICE_NET_AMOUNT_EUR = "valoareeur";
    private static final String CPV_CODE_ID = "cpvcodeid";
    private static final String CPV_CODE = "cpvcode";
    private static final String NOTICE_PUBLICATION_BUYER_ASSIGNED_ID = "numaranuntparticipare";
    private static final String NOTICE_PUBLICATION_DATE = "dataanuntparticipare";
    private static final String TENDER_ESTIMATED_PRICE_NET_AMOUNT = "valoareestimataparticipare";
    private static final String TENDER_ESTIMATED_PRICE_CURRENCY = "monedavaloareestimataparticipare";
    private static final String TENDER_ESTIMATED_PRICE_CURRENCY_SECOND_FORM = "monedavaloareestimatapart";
    private static final String FUNDING_IS_EU_FUND = "fonduricomunitare";
    private static final String FUNDING_SOURCE = "tipfinantare";
    private static final String TIP_LEGISLATIE_ID = "tiplegislatieid";
    private static final String TIP_LEGISLATIE_ID_NEW = "tiplegislatie";
    private static final String FUNDING_PROGRAMME = "fondeuropean";
    private static final String CONTRACT_PERIODIC = "contractperiodic";
    private static final String TENDER_DEPOSITS = "depozitegarantii";
    private static final String FUNDING_SOURCE2 = "modalitatifinantare"; // has similar description as FUNDING_SOURCE
    private static final String MODALITATE_CONTRACTARE = "modalitateccontractare"; // only in 2019, no description

    /**
     * Private constructor for noninstantiability.
     */
    private APATenderExcelContracteHandler(){

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
            headerIndexes.put(headerRow.getCell(i).getStringCellValue(), i);
        }
        final List<ParsedTender> parsedTenders = new ArrayList<>();
        Row row;
        for (int i = 0; i <= sheet.getLastRowNum(); i++) {
            row = sheet.getRow(i);


            String bidsCount = checkingValue(headerIndexes.get(BIDS_COUNT), row);
            if (bidsCount == null) {
                bidsCount = checkingValue(headerIndexes.get(BIDS_COUNT_SECOND_FORM), row);
            }
            String bidderName = checkingValue(headerIndexes.get(BIDDER_NAME), row);
            String bidderId = checkingValue(headerIndexes.get(BIDDER_IDS), row);

            String bidderCountry = checkingValue(headerIndexes.get(BIDDER_COUNTRY), row);
            String bidderCity = checkingValue(headerIndexes.get(BIDDER_CITY), row);
            String bidderRawAddress = checkingValue(headerIndexes.get(BIDDER_RAW_ADDRESS), row);

            String isSubcontract = checkingValue(headerIndexes.get(IS_SUBCONTRACTATED), row);

            String publicationDate = checkingValue(headerIndexes.get(AWARD_PUBLICATION_DATE), row);
            String publicationDate2 = checkingValue(headerIndexes.get(NOTICE_PUBLICATION_DATE), row);
            String contractSignatureDate = checkingValue(headerIndexes.get(CONTRACT_SIGNATURE_DATE), row);
            String priceNetAmount = checkingValue(headerIndexes.get(BID_PRICE_NET_AMOUNT), row);
            if(priceNetAmount == null){
                priceNetAmount = checkingValue(headerIndexes.get(BID_PRICE_NET_AMOUNT_SECOND_FORM), row);
            }
            String priceCurrency = checkingValue(headerIndexes.get(BID_PRICE_CURRENCY), row);
            String priceNetAmountEur = checkingValue(headerIndexes.get(BID_PRICE_NET_AMOUNT_EUR), row);
            String estimatedPriceNetAmount = checkingValue(headerIndexes.get(TENDER_ESTIMATED_PRICE_NET_AMOUNT), row);
            String estimatedPriceCurrency = checkingValue(headerIndexes.get(TENDER_ESTIMATED_PRICE_CURRENCY), row);
            if(estimatedPriceCurrency == null){
                estimatedPriceCurrency = checkingValue(headerIndexes.get(TENDER_ESTIMATED_PRICE_CURRENCY_SECOND_FORM), row);
            }
            String procedureType = checkingValue(headerIndexes.get(PROCEDURE_TYPE), row);

            String buyerName = checkingValue(headerIndexes.get(BUYER_NAME), row);
            String buyerId = checkingValue(headerIndexes.get(BUYER_IDS), row);
            String buyerType = checkingValue(headerIndexes.get(BUYER_TYPE), row);
            String buyerActivity = checkingValue(headerIndexes.get(BUYER_MAIN_ACTIVITIES), row);

            String selectionMethod = checkingValue(headerIndexes.get(SELECTION_METHOD), row);
            String buyerAssignedId = checkingValue(headerIndexes.get(LOT_BUYER_ASSIGNED_ID), row);
            String buyerAssignedIdPublication = checkingValue(headerIndexes.get(AWARD_PUBLICATION_BUYER_ASSIGNED_ID), row);
            String buyerAssignedIdPublication2 = checkingValue(headerIndexes.get(NOTICE_PUBLICATION_BUYER_ASSIGNED_ID), row);

            String supplyType = checkingValue(headerIndexes.get(SUPPLY_TYPE), row);
            String isEuFund = checkingValue(headerIndexes.get(FUNDING_IS_EU_FUND), row);
            String cpvCode = checkingValue(headerIndexes.get(CPV_CODE), row);

            String source = checkingValue(headerIndexes.get(FUNDING_SOURCE), row);
            String program = checkingValue(headerIndexes.get(FUNDING_PROGRAMME), row);
            String deposits = checkingValue(headerIndexes.get(TENDER_DEPOSITS), row);
            String title = checkingValue(headerIndexes.get(TITLE), row);
            String isFrameworkAgreement = checkingValue(headerIndexes.get(IS_FRAMEWORK_AGREEMENT), row);
            String isElectronicAuction = checkingValue(headerIndexes.get(IS_ELECTRONIC_AUCTION), row);


            parsedTenders.add(new ParsedTender()
                    .addLot(new ParsedTenderLot()
                            .setBidsCount(bidsCount)
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
                                    .setIsWinning(Boolean.TRUE.toString())
                                    .setIsSubcontracted(isSubcontract)
                                    .setPrice(new ParsedPrice()
                                            .setNetAmount(priceNetAmount)
                                            .setCurrency(priceCurrency)
                                            .setNetAmountEur(priceNetAmountEur)))
                            .setContractSignatureDate(contractSignatureDate))
                    .setEstimatedPrice(new ParsedPrice()
                            .setNetAmount(estimatedPriceNetAmount)
                            .setCurrency(estimatedPriceCurrency)
                    )
                    .setNationalProcedureType(procedureType)
                    .setProcedureType(procedureType)
                    .addBuyer(new ParsedBody()
                            .setName(buyerName)
                            .setBuyerType(buyerType)
                            .addMainActivity(buyerActivity)
                            .addBodyId(new BodyIdentifier()
                                    .setId(buyerId)
                                    .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                    .setScope(BodyIdentifier.Scope.RO)))
                    .setBuyerAssignedId(buyerAssignedId)
                    .setSupplyType(supplyType)
                    .addPublication(new ParsedPublication()
                            .setBuyerAssignedId(buyerAssignedIdPublication)
                            .setPublicationDate(publicationDate)
                            .setSourceFormType("CONTRACT_AWARD")
                            .setIsIncluded(true)
                            .setSource(PublicationSources.RO_APA))
                    .addPublication(new ParsedPublication()
                            .setBuyerAssignedId(buyerAssignedIdPublication2)
                            .setPublicationDate(publicationDate2)
                            .setFormType("CONTRACT_NOTICE")
                            .setIsIncluded(false)
                            .setSource(PublicationSources.RO_APA))
                    .addFunding(new ParsedFunding()
                            .setIsEuFund(isEuFund)
                            .setSource(source)
                            .setProgramme(program))
                    .setDeposits(deposits)
                    .setTitle(title)
                    .setIsFrameworkAgreement(isFrameworkAgreement)
                    .setIsElectronicAuction(isElectronicAuction)
                    .setSelectionMethod(selectionMethod)
                    .addCpv(new ParsedCPV()
                            .setIsMain(Boolean.TRUE.toString())
                            .setCode(cpvCode)));
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


