package eu.datlab.worker.ro.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.slf4j.Logger;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Parser handler for RO source using ANNOTATION_directcontract_2007.xlsx and ANNOTATION_directcontract_2019.xlsx.
 */
public final class APATenderCsvDirectContractHandler {
    private static final String CASTIGATOR = "Castigator";
    private static final String CASTIGATOR_CUI = "CastigatorCUI";
    private static final String CASTIGATOR_TARA = "CastigatorTara";
    private static final String CASTIGATOR_LOCALITATE = "CastigatorLocalitate";
    private static final String CASTIGATOR_ADRESA = "CastigatorAdresa";
    private static final String TIP_PROCEDURA = "TipProcedura";
    private static final String AUTORITATE_CONTRACTANTA = "AutoritateContractanta";
    private static final String AUTORITATE_CONTRACTANTA_CU = "AutoritateContractantaCU";
    private static final String NUMAR_ANUNT = "NumarAnunt";
    private static final String DATA_ANUNT = "DataAnunt";
    private static final String TIP_INCHEIERE_CONTRACT = "TipIncheiereContract";
    private static final String NUMAR_CONTRACT = "NumarContract";
    private static final String DATA_CONTRACT = "DataContract";
    private static final String TITLU_CONTRACT = "TitluContract";
    private static final String VALOARE = "Valoare";
    private static final String MONEDA = "Moneda";
    private static final String VALOARE_RON = "ValoareRON";
    private static final String VALOARE_EUR = "ValoareEUR";
    private static final String CPV_CODE_ID = "CPVCodeID";
    private static final String CPV_CODE = "CPVCode";
    private static final String DESCRIERE = "DESCRIERE";  // only in 2019

    private static final String[] CSV_OLD_HEADER = {CASTIGATOR, CASTIGATOR_CUI, CASTIGATOR_TARA, CASTIGATOR_LOCALITATE,
            CASTIGATOR_ADRESA, TIP_PROCEDURA, AUTORITATE_CONTRACTANTA, AUTORITATE_CONTRACTANTA_CU, NUMAR_ANUNT,
            DATA_ANUNT, TIP_INCHEIERE_CONTRACT, NUMAR_CONTRACT, DATA_CONTRACT, TITLU_CONTRACT, VALOARE, MONEDA,
            VALOARE_RON, VALOARE_EUR, CPV_CODE_ID, CPV_CODE};

    private static final String[] CSV_NEW_HEADER_1 = {CASTIGATOR, CASTIGATOR_CUI, CASTIGATOR_TARA, CASTIGATOR_LOCALITATE,
            CASTIGATOR_ADRESA, TIP_PROCEDURA, AUTORITATE_CONTRACTANTA, AUTORITATE_CONTRACTANTA_CU, NUMAR_ANUNT,
            DATA_ANUNT, TIP_INCHEIERE_CONTRACT, NUMAR_CONTRACT, DATA_CONTRACT, TITLU_CONTRACT, VALOARE, MONEDA,
            VALOARE_RON, VALOARE_EUR, CPV_CODE_ID, CPV_CODE, DESCRIERE};

    private static final String[] CSV_NEW_HEADER_2 = {CASTIGATOR, CASTIGATOR_CUI, CASTIGATOR_TARA, CASTIGATOR_LOCALITATE,
            CASTIGATOR_ADRESA, TIP_PROCEDURA, AUTORITATE_CONTRACTANTA, AUTORITATE_CONTRACTANTA_CU, NUMAR_ANUNT,
            DATA_ANUNT, DESCRIERE, TIP_INCHEIERE_CONTRACT, NUMAR_CONTRACT, DATA_CONTRACT, TITLU_CONTRACT, VALOARE,
            MONEDA, VALOARE_RON, VALOARE_EUR, CPV_CODE_ID, CPV_CODE};

    private static final CSVFormat CSV_OLD_FORMAT = CSVFormat.newFormat('^').withHeader(CSV_OLD_HEADER).withIgnoreEmptyLines(true);
    private static final CSVFormat CSV_NEW_FORMAT_1 =
            CSVFormat.newFormat('^').withHeader(CSV_NEW_HEADER_1).withIgnoreEmptyLines(true);
    private static final CSVFormat CSV_NEW_FORMAT_2 =
            CSVFormat.newFormat('^').withHeader(CSV_NEW_HEADER_2).withIgnoreEmptyLines(true);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private APATenderCsvDirectContractHandler() {
    }

    /**
     * @param raw    raw data
     * @param logger logger
     * @return List<ParsedTender>
     */
    public static List<ParsedTender> parse(final RawData raw, final Logger logger) {
        try {
            final List<ParsedTender> parsedTenders = new ArrayList<>();

            boolean isNew = false;
            CSVFormat format = null;
            APACSVReader reader;
            if (raw.getSourceData().split("\n", 2)[0].toLowerCase().contains("descriere")) {
                // length of both new formats are the same
                reader = new APACSVReader(raw.getSourceData(), CSV_NEW_HEADER_1.length);
                isNew = true;
                // DESCRIERE is in the middle of the header
                if(raw.getSourceData().split("\n", 2)[0].split("(?i)descriere")[1].contains("^")) {
                    format = CSV_NEW_FORMAT_2;
                } else {
                    format = CSV_NEW_FORMAT_1;
                }
            } else {
                reader = new APACSVReader(raw.getSourceData(), CSV_OLD_HEADER.length);
                format = CSV_OLD_FORMAT;
            }

            reader.readLine(); // skip header

            String line;
            while ((line = reader.readLine()) != null) {
                if(line.isEmpty()) {
                    continue;
                }
                CSVParser csvParser = CSVParser.parse(line, format);

                for (CSVRecord tender : csvParser) {
                    parsedTenders.add(new ParsedTender()
                            .addLot(new ParsedTenderLot()
                                    .addBid(new ParsedBid()
                                            .addBidder(new ParsedBody()
                                                    .setName(tender.get(CASTIGATOR))
                                                    .addBodyId(new BodyIdentifier()
                                                            .setId(tender.get(CASTIGATOR_CUI))
                                                            .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                                            .setScope(BodyIdentifier.Scope.RO))
                                                    .setAddress(new ParsedAddress()
                                                            .setCountry(tender.get(CASTIGATOR_TARA))
                                                            .setCity(tender.get(CASTIGATOR_LOCALITATE))
                                                            .setRawAddress(tender.get(CASTIGATOR_ADRESA))))
                                            .setIsWinning(Boolean.TRUE.toString())
                                            .setPrice(new ParsedPrice()
                                                    .setNetAmount(tender.get(VALOARE))
                                                    .setCurrency(tender.get(MONEDA))
                                                    .setNetAmountEur(tender.get(VALOARE_EUR))))
                                    .setContractSignatureDate(tender.get(DATA_CONTRACT)))
                            .setNationalProcedureType(tender.get(TIP_PROCEDURA))
                            .addBuyer(new ParsedBody()
                                    .setName(tender.get(AUTORITATE_CONTRACTANTA))
                                    .addBodyId(new BodyIdentifier()
                                            .setId(tender.get(AUTORITATE_CONTRACTANTA_CU))
                                            .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                            .setScope(BodyIdentifier.Scope.RO)))
                            .setBuyerAssignedId(tender.get(NUMAR_CONTRACT))
                            .addPublication(new ParsedPublication()
                                    .setBuyerAssignedId(tender.get(NUMAR_ANUNT))
                                    .setPublicationDate(tender.get(DATA_ANUNT))
                                    .setIsIncluded(true)
                                    .setSourceFormType(String.valueOf(PublicationFormType.CONTRACT_AWARD))
                                    .setSource(PublicationSources.RO_APA))
                            .setTitle(tender.get(TITLU_CONTRACT))
                            .addCpv(new ParsedCPV()
                                    .setIsMain(Boolean.TRUE.toString())
                                    .setCode(tender.get(CPV_CODE)))
                            .setDescription(isNew ? tender.get(DESCRIERE) : null));
                }
            }

            return parsedTenders;
        } catch (IOException e) {
            logger.error("Parsing failed with exception {}", e);
            throw new UnrecoverableException("Unable to parse page", e);
        }
    }
}
