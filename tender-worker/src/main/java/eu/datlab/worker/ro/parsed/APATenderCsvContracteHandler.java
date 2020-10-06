package eu.datlab.worker.ro.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.slf4j.Logger;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;


/**
 * Parser handler for RO source using ANNOTATION_contracte_2007.xlsx, ANNOTATION_contracte_2019.xlsx,
 * ANNOTATION_subsequent_contract_2016 and ANNOTATION_subsequent_contract_2018.
 */
public final class APATenderCsvContracteHandler {
    private static final String BIDDER_NAME = "Castigator";
    private static final String BIDDER_IDS = "CastigatorCUI";
    private static final String BIDDER_COUNTRY = "CastigatorTara";
    private static final String BIDDER_CITY = "CastigatorLocalitate";
    private static final String BIDDER_ADDRESS = "CastigatorAdresa";
    private static final String FORM_TYPE = "Tip";  // not used
    private static final String SUPPLY_TYPE = "TipContract";
    private static final String PROCEDURE_TYPE = "TipProceduer";
    private static final String BUYER_NAME = "AutoritateContractanta";
    private static final String BUYER_IDS = "AutoritateContractantaCU";
    private static final String BUYER_TYPE = "TipAC";
    private static final String BUYER_MAIN_ACTIVITIES = "TipActivitateAC";
    private static final String AWARD_PUBLICATION_BUYER_ASSIGNED_ID = "NumarAnuntAtribuire";
    private static final String AWARD_PUBLICATION_PUBLICATION_DATE = "DataAnuntAtribuire";
    private static final String IS_FRAMEWORK_AGREEMENT = "TipIncheiereContract";
    private static final String SELECTION_METHOD = "TipCriteriiAtribuire";
    private static final String IS_ELECTRONIC_AUCTION = "CuLicitatieElectronica";
    private static final String BIDS_COUNT = "NumarOfertePrimite";
    private static final String SUBCONTRACT = "Subcontractat";
    private static final String LOT_BUYER_ASSIGNED_ID = "NumarContract";
    private static final String CONTRACT_SIGNATURE_DATE = "DataContract";
    private static final String TITLE = "TitluContract";
    private static final String BID_PRICE_NET_AMOUNT = "Valoare";
    private static final String BID_PRICE_CURRENCY = "Moneda";
    private static final String BID_PRICE_NET_AMOUNT_NATIONAL = "ValoareRON";  // not used
    private static final String BID_PRICE_NET_AMOUNT_EUR = "ValoareEUR";
    private static final String CPV_CODE_ID = "CPVCodeID";
    private static final String CPV_CODE = "CPVCode";
    private static final String NOTICE_PUBLICATION_BUYER_ASSIGNED_ID = "NumarAnuntParticipare";
    private static final String NOTICE_PUBLICATION_PUBLICATION_DATE = "DataAnuntParticipare";
    private static final String BID_ESTIMATED_PRICE_NET_AMOUNT = "ValoareEstimataParticipare";
    private static final String BID_ESTIMATED_PRICE_CURRENCY = "MonedaValoareEstimataParticipare";
    private static final String IS_EU_FUND = "FonduriComunitare";
    private static final String FUNDING_SOURCE = "TipFinantare";
    private static final String LEGISLATIE_TYPE_ID = "TipLegislatieID";
    private static final String FUNDING_PROGRAMME = "FondEuropean";
    private static final String CONTRACT_PERIODIC = "ContractPeriodic";
    private static final String TENDER_DEPOSITS = "DepoziteGarantii";
    private static final String FUNDING_SOURCE2 = "ModalitatiFinantare";  // has the same description as FUNDING_SOURCE
    private static final String MODALITATE_CONTRACTARE = "ModalitateContractare";

    private static final String[] CSV_OLD_HEADER = {BIDDER_NAME, BIDDER_IDS, BIDDER_COUNTRY, BIDDER_CITY,
            BIDDER_ADDRESS, FORM_TYPE, SUPPLY_TYPE, PROCEDURE_TYPE, BUYER_NAME, BUYER_IDS, BUYER_TYPE,
            BUYER_MAIN_ACTIVITIES, AWARD_PUBLICATION_BUYER_ASSIGNED_ID, AWARD_PUBLICATION_PUBLICATION_DATE,
            IS_FRAMEWORK_AGREEMENT, SELECTION_METHOD, IS_ELECTRONIC_AUCTION, BIDS_COUNT, SUBCONTRACT,
            LOT_BUYER_ASSIGNED_ID, CONTRACT_SIGNATURE_DATE, TITLE, BID_PRICE_NET_AMOUNT, BID_PRICE_CURRENCY,
            BID_PRICE_NET_AMOUNT_NATIONAL, BID_PRICE_NET_AMOUNT_EUR, CPV_CODE_ID, CPV_CODE,
            NOTICE_PUBLICATION_BUYER_ASSIGNED_ID, NOTICE_PUBLICATION_PUBLICATION_DATE, BID_ESTIMATED_PRICE_NET_AMOUNT,
            BID_ESTIMATED_PRICE_CURRENCY, IS_EU_FUND, FUNDING_SOURCE, LEGISLATIE_TYPE_ID, FUNDING_PROGRAMME,
            CONTRACT_PERIODIC, TENDER_DEPOSITS, FUNDING_SOURCE2}; // 39

    private static final String[] CSV_NEW_CONTRACTE_HEADER = {BIDDER_NAME, BIDDER_IDS, BIDDER_COUNTRY, BIDDER_CITY,
            BIDDER_ADDRESS, FORM_TYPE, SUPPLY_TYPE, PROCEDURE_TYPE, BUYER_NAME, BUYER_IDS, BUYER_TYPE,
            BUYER_MAIN_ACTIVITIES, AWARD_PUBLICATION_BUYER_ASSIGNED_ID, AWARD_PUBLICATION_PUBLICATION_DATE,
            IS_FRAMEWORK_AGREEMENT, SELECTION_METHOD, IS_ELECTRONIC_AUCTION, BIDS_COUNT, SUBCONTRACT,
            LOT_BUYER_ASSIGNED_ID, CONTRACT_SIGNATURE_DATE, TITLE, BID_PRICE_NET_AMOUNT, BID_PRICE_CURRENCY,
            BID_PRICE_NET_AMOUNT_NATIONAL, CPV_CODE_ID, CPV_CODE,
            NOTICE_PUBLICATION_BUYER_ASSIGNED_ID, NOTICE_PUBLICATION_PUBLICATION_DATE, BID_ESTIMATED_PRICE_NET_AMOUNT,
            BID_ESTIMATED_PRICE_CURRENCY, IS_EU_FUND, FUNDING_SOURCE, LEGISLATIE_TYPE_ID, FUNDING_PROGRAMME,
            CONTRACT_PERIODIC, TENDER_DEPOSITS, FUNDING_SOURCE2, MODALITATE_CONTRACTARE}; // 39

    private static final String[] CSV_NEW_SUBSEQUENT_HEADER = {BIDDER_NAME, BIDDER_IDS, BIDDER_COUNTRY, BIDDER_CITY,
            BIDDER_ADDRESS, FORM_TYPE, SUPPLY_TYPE, PROCEDURE_TYPE, BUYER_NAME, BUYER_IDS, BUYER_TYPE,
            BUYER_MAIN_ACTIVITIES, AWARD_PUBLICATION_BUYER_ASSIGNED_ID, AWARD_PUBLICATION_PUBLICATION_DATE,
            IS_FRAMEWORK_AGREEMENT, SELECTION_METHOD, IS_ELECTRONIC_AUCTION, LOT_BUYER_ASSIGNED_ID,
            CONTRACT_SIGNATURE_DATE, TITLE, BID_PRICE_NET_AMOUNT, BID_PRICE_CURRENCY,
            BID_PRICE_NET_AMOUNT_NATIONAL, BID_PRICE_NET_AMOUNT_EUR, CPV_CODE_ID, CPV_CODE,
            NOTICE_PUBLICATION_BUYER_ASSIGNED_ID, NOTICE_PUBLICATION_PUBLICATION_DATE, BID_ESTIMATED_PRICE_NET_AMOUNT,
            BID_ESTIMATED_PRICE_CURRENCY, IS_EU_FUND, FUNDING_SOURCE, LEGISLATIE_TYPE_ID, FUNDING_PROGRAMME,
            CONTRACT_PERIODIC, TENDER_DEPOSITS, FUNDING_SOURCE2}; // 37

    private static final String[] CSV_NEW_NO_EUR_SUBSEQUENT_HEADER = {BIDDER_NAME, BIDDER_IDS, BIDDER_COUNTRY, BIDDER_CITY,
            BIDDER_ADDRESS, FORM_TYPE, SUPPLY_TYPE, PROCEDURE_TYPE, BUYER_NAME, BUYER_IDS, BUYER_TYPE,
            BUYER_MAIN_ACTIVITIES, AWARD_PUBLICATION_BUYER_ASSIGNED_ID, AWARD_PUBLICATION_PUBLICATION_DATE,
            IS_FRAMEWORK_AGREEMENT, SELECTION_METHOD, IS_ELECTRONIC_AUCTION, LOT_BUYER_ASSIGNED_ID,
            CONTRACT_SIGNATURE_DATE, TITLE, BID_PRICE_NET_AMOUNT, BID_PRICE_CURRENCY,
            BID_PRICE_NET_AMOUNT_NATIONAL, CPV_CODE_ID, CPV_CODE,
            NOTICE_PUBLICATION_BUYER_ASSIGNED_ID, NOTICE_PUBLICATION_PUBLICATION_DATE, BID_ESTIMATED_PRICE_NET_AMOUNT,
            BID_ESTIMATED_PRICE_CURRENCY, IS_EU_FUND, FUNDING_SOURCE, LEGISLATIE_TYPE_ID, FUNDING_PROGRAMME,
            CONTRACT_PERIODIC, TENDER_DEPOSITS, FUNDING_SOURCE2}; // 36

    private static final CSVFormat CSV_NEW_SUBSEQUENT_FORMAT = CSVFormat.newFormat('^').
            withHeader(CSV_NEW_SUBSEQUENT_HEADER).withIgnoreEmptyLines(true);
    private static final CSVFormat CSV_OLD_FORMAT = CSVFormat.newFormat('^').withHeader(CSV_OLD_HEADER).withIgnoreEmptyLines(true);
    private static final CSVFormat CSV_NEW_CONTRACTE_FORMAT = CSVFormat.newFormat('^').
            withHeader(CSV_NEW_CONTRACTE_HEADER).withIgnoreEmptyLines(true);
    private static final CSVFormat CSV_NEW_NO_EUR_SUBSEQUENT_FORMAT = CSVFormat.newFormat('^').
            withHeader(CSV_NEW_NO_EUR_SUBSEQUENT_HEADER).withIgnoreEmptyLines(true);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private APATenderCsvContracteHandler() {
    }

    /**
     * Parses the given raw data.
     * @param raw raw data
     * @param logger logger
     * @return list of parsed tenders
     */
    public static List<ParsedTender> parse(final RawData raw, final Logger logger) {
        try {
            final List<ParsedTender> parsedTenders = new ArrayList<>();

            boolean isNewContracte = false, isNewSubsequente = false, isNSNoEur = false, isOld = false;
            APACSVReader reader;
            String headerLine = raw.getSourceData().split("\n", 2)[0];
            if (headerLine.contains(MODALITATE_CONTRACTARE)) {
                reader = new APACSVReader(raw.getSourceData(), CSV_NEW_CONTRACTE_HEADER.length);
                isNewContracte = true;
            } else if (headerLine.contains(SUBCONTRACT)) {
                reader = new APACSVReader(raw.getSourceData(), CSV_OLD_HEADER.length);
                isOld = true;
            } else if(!headerLine.contains(BID_PRICE_NET_AMOUNT_EUR)) {
                reader = new APACSVReader(raw.getSourceData(), CSV_NEW_NO_EUR_SUBSEQUENT_HEADER.length);
                isNSNoEur = true;
            } else{
                reader = new APACSVReader(raw.getSourceData(), CSV_NEW_SUBSEQUENT_HEADER.length);
                isNewSubsequente = true;
            }

            reader.readLine(); // skip header

            String formType = String.valueOf(PublicationFormType.CONTRACT_AWARD);
            if(raw.getSourceUrl().getFile().contains("subsecvente")){
                formType = String.valueOf(PublicationFormType.CONTRACT_IMPLEMENTATION);
            }

            String line;
            while ((line = reader.readLine()) != null) {
                if(line.isEmpty()) {
                    continue;
                }
                CSVParser csvParser;
                if (isNewContracte) {
                    csvParser = CSVParser.parse(line, CSV_NEW_CONTRACTE_FORMAT);
                } else if (isNewSubsequente) {
                    csvParser = CSVParser.parse(line, CSV_NEW_SUBSEQUENT_FORMAT);
                } else if(isNSNoEur){
                    csvParser = CSVParser.parse(line, CSV_NEW_NO_EUR_SUBSEQUENT_FORMAT);
                } else {
                    csvParser = CSVParser.parse(line, CSV_OLD_FORMAT);
                }

                for (CSVRecord tender : csvParser) {
                    parsedTenders.add(new ParsedTender()
                            .addLot(new ParsedTenderLot()
                                    .setBidsCount(isNewSubsequente || isNSNoEur ? null : tender.get(BIDS_COUNT))
                                    .addBid(new ParsedBid()
                                            .addBidder(new ParsedBody()
                                                    .setName(tender.get(BIDDER_NAME))
                                                    .addBodyId(new BodyIdentifier()
                                                            .setId(tender.get(BIDDER_IDS))
                                                            .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                                            .setScope(BodyIdentifier.Scope.RO))
                                                    .setAddress(new ParsedAddress()
                                                            .setCountry(tender.get(BIDDER_COUNTRY))
                                                            .setCity(tender.get(BIDDER_CITY))
                                                            .setRawAddress(tender.get(BIDDER_ADDRESS))))
                                            .setIsWinning(Boolean.TRUE.toString())
                                            .setIsSubcontracted(isNewSubsequente || isNSNoEur ? null : tender.get(SUBCONTRACT))
                                            .setPrice(new ParsedPrice()
                                                    .setNetAmount(tender.get(BID_PRICE_NET_AMOUNT))
                                                    .setCurrency(tender.get(BID_PRICE_CURRENCY))
                                                    .setNetAmountEur(isNewContracte || isNSNoEur
                                                            ? null : tender.get(BID_PRICE_NET_AMOUNT_EUR))))
                                    .setContractSignatureDate(tender.get(CONTRACT_SIGNATURE_DATE)))
                            .setNationalProcedureType(tender.get(PROCEDURE_TYPE))
                            .setSupplyType(tender.get(SUPPLY_TYPE))
                            .setSelectionMethod(tender.get(SELECTION_METHOD))
                            .addBuyer(new ParsedBody()
                                    .setName(tender.get(BUYER_NAME))
                                    .setBuyerType(tender.get(BUYER_TYPE))
                                    .addMainActivity(tender.get(BUYER_MAIN_ACTIVITIES))
                                    .setBuyerType(tender.get(BUYER_TYPE))
                                    .addBodyId(new BodyIdentifier()
                                            .setId(tender.get(BUYER_IDS))
                                            .setScope(BodyIdentifier.Scope.RO)))
                            .setBuyerAssignedId(tender.get(LOT_BUYER_ASSIGNED_ID))
                            .addPublication(new ParsedPublication()
                                    .setBuyerAssignedId(tender.get(AWARD_PUBLICATION_BUYER_ASSIGNED_ID))
                                    .setPublicationDate(tender.get(AWARD_PUBLICATION_PUBLICATION_DATE))
                                    .setIsIncluded(true)
                                    .setSourceFormType(formType)
                                    .setSource(PublicationSources.RO_APA))
                            .addPublication(new ParsedPublication()
                                    .setBuyerAssignedId(tender.get(NOTICE_PUBLICATION_BUYER_ASSIGNED_ID))
                                    .setPublicationDate(tender.get(NOTICE_PUBLICATION_PUBLICATION_DATE))
                                    .setIsIncluded(false)
                                    .setSourceFormType(String.valueOf(PublicationFormType.CONTRACT_NOTICE))
                                    .setSource(PublicationSources.RO_APA))
                            .setEstimatedPrice(new ParsedPrice()
                                    .setNetAmount(tender.get(BID_ESTIMATED_PRICE_NET_AMOUNT))
                                    .setCurrency(tender.get(BID_ESTIMATED_PRICE_CURRENCY)))
                            .setTitle(tender.get(TITLE))
                            .addCpv(new ParsedCPV()
                                    .setIsMain(Boolean.TRUE.toString())
                                    .setCode(tender.get(CPV_CODE)))
                            .setIsFrameworkAgreement(tender.get(IS_FRAMEWORK_AGREEMENT))
                            .setIsElectronicAuction(tender.get(IS_ELECTRONIC_AUCTION))
                            .addFunding(new ParsedFunding()
                                    .setIsEuFund(tender.get(IS_EU_FUND))
                                    .setSource(tender.get(FUNDING_SOURCE))
                                    .setProgramme(tender.get(FUNDING_PROGRAMME)))
                            .setDeposits(tender.get(TENDER_DEPOSITS)));
                }
            }
            return parsedTenders;
        } catch (IOException e) {
            logger.error("Parsing failed with exception {}", e);
            throw new UnrecoverableException("Unable to parse page", e);
        }
    }
}
