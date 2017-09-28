package eu.digiwhist.worker.ro.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
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
 * Parser handler for RO source.
 */
public final class APATenderFormTwoHandler {
    private static final String CASTIGATOR = "Castigator";
    private static final String CASTIGATOR_CUI = "CastigatorCUI";
    private static final String CASTIGATOR_TARA = "CastigatorTara";
    private static final String CASTIGATOR_LOCALITATE = "CastigatorLocalitate";
    private static final String CASTIGATOR_ADRESA = "CastigatorAdresa";
    private static final String TIP = "Tip";
    private static final String TIP_CONTRACT = "TipContract";
    private static final String TIP_PROCEDURA = "TipProcedura";
    private static final String AUTORITATE_CONTRACTANTA = "AutoritateContractanta";
    private static final String AUTORITATE_CONTRACTANTA_CUI = "AutoritateContractantaCUI";
    private static final String TIP_AC = "TipAC";
    private static final String TIP_ACTIVITATE_AC = "TipActivitateAC";
    private static final String NUMAR_ANUNT_ATRIBUIRE = "NumarAnuntAtribuire";
    private static final String DATA_ANUNT_ATRIBUIRE = "DataAnuntAtribuire";
    private static final String TIP_INCHEIERE_CONTRACT = "TipIncheiereContract";
    private static final String TIP_CRITERII_ATRIBUIRE = "TipCriteriiAtribuire";
    private static final String CU_LICITATIE_ELECTRONICA = "CuLicitatieElectronica";
    private static final String NUMAR_OFERTE_PRIMITE = "NumarOfertePrimite";
    private static final String SUBCONTRACTAT = "Subcontractat";
    private static final String NUMAR_CONTRACT = "NumarContract";
    private static final String DATA_CONTRACT = "DataContract";
    private static final String TITLU_CONTRACT = "TitluContract";
    private static final String VALOARE = "Valoare";
    private static final String MONEDA = "Moneda";
    private static final String VALOARE_RON = "ValoareRON";
    private static final String VALOARE_EUR = "ValoareEUR";
    private static final String CPV_CODE_ID = "CPVCodeID";
    private static final String CPV_CODE = "CPVCode";
    private static final String NUMAR_ANUNT_PARTICIPARE = "NumarAnuntParticipare";
    private static final String DATA_ANUNT_PARTICIPARE = "DataAnuntParticipare";
    private static final String VALOARE_ESTIMATA_PARTICIPARE = "ValoareEstimataParticipare";
    private static final String MONEDA_VALOARE_ESTIMATA_PARTICIPARE = "MonedaValoareEstimataParticipare";
    private static final String FONDURI_COMUNITARE = "FonduriComunitare";
    private static final String TIP_FINANTARE = "TipFinantare";
    private static final String TIP_LEGISLATIE_ID = "TipLegislatieID";
    private static final String FOND_EUROPEAN = "FondEuropean";
    private static final String CONTRACT_PERIODIC = "ContractPeriodic";
    private static final String DEPOZITE_GARANTII = "DepoziteGarantii";
    private static final String MODALITATI_FINANTARE = "ModalitatiFinantare";

    private static final String[] CSV_HEADER = {CASTIGATOR, CASTIGATOR_CUI, CASTIGATOR_TARA, CASTIGATOR_LOCALITATE,
            CASTIGATOR_ADRESA, TIP, TIP_CONTRACT, TIP_PROCEDURA, AUTORITATE_CONTRACTANTA, AUTORITATE_CONTRACTANTA_CUI,
            TIP_AC, TIP_ACTIVITATE_AC, NUMAR_ANUNT_ATRIBUIRE, DATA_ANUNT_ATRIBUIRE, TIP_INCHEIERE_CONTRACT,
            TIP_CRITERII_ATRIBUIRE, CU_LICITATIE_ELECTRONICA, NUMAR_OFERTE_PRIMITE, SUBCONTRACTAT, NUMAR_CONTRACT,
            DATA_CONTRACT, TITLU_CONTRACT, VALOARE, MONEDA, VALOARE_RON, VALOARE_EUR, CPV_CODE_ID, CPV_CODE,
            NUMAR_ANUNT_PARTICIPARE, DATA_ANUNT_PARTICIPARE, VALOARE_ESTIMATA_PARTICIPARE,
            MONEDA_VALOARE_ESTIMATA_PARTICIPARE, FONDURI_COMUNITARE, TIP_FINANTARE, TIP_LEGISLATIE_ID, FOND_EUROPEAN,
            CONTRACT_PERIODIC, DEPOZITE_GARANTII, MODALITATI_FINANTARE};

    /**
     * Suppress default constructor for noninstantiability.
     */
    private APATenderFormTwoHandler() {
    }

    /**
     * @param raw    raw data
     * @param logger logger
     *
     * @return List<ParsedTender>
     */
    public static List<ParsedTender> parse(final RawData raw, final Logger logger) {
        final CSVParser csvParser;
        try {
            final List<ParsedTender> parsedTenders = new ArrayList<>();
            csvParser = CSVParser.parse(raw.getSourceData(),
                    CSVFormat.newFormat('^').withHeader(CSV_HEADER).withSkipHeaderRecord(true));
            final List<CSVRecord> tenders = csvParser.getRecords();

            for (CSVRecord tender : tenders) {
                parsedTenders.add(new ParsedTender()
                        .addLot(new ParsedTenderLot()
                                .setBidsCount(tender.get(NUMAR_OFERTE_PRIMITE))
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
                                        .setIsSubcontracted(tender.get(SUBCONTRACTAT))
                                        .setPrice(new ParsedPrice()
                                                .setNetAmount(tender.get(VALOARE))
                                                .setCurrency(tender.get(MONEDA))
                                                .setNetAmountEur(tender.get(VALOARE_EUR))))
                                .setContractSignatureDate(tender.get(DATA_CONTRACT)))
                        .setEstimatedPrice(new ParsedPrice()
                                .setNetAmount(tender.get(VALOARE_ESTIMATA_PARTICIPARE))
                                .setCurrency(tender.get(MONEDA_VALOARE_ESTIMATA_PARTICIPARE))
                        )
                        .setNationalProcedureType(tender.get(TIP_PROCEDURA))
                        .setProcedureType(tender.get(TIP_PROCEDURA))
                        .addBuyer(new ParsedBody()
                                .setName(tender.get(AUTORITATE_CONTRACTANTA))
                                .setBuyerType(tender.get(TIP_AC))
                                .addMainActivity(tender.get(TIP_ACTIVITATE_AC))
                                .addBodyId(new BodyIdentifier()
                                        .setId(tender.get(AUTORITATE_CONTRACTANTA_CUI))
                                        .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                                        .setScope(BodyIdentifier.Scope.RO)))
                        .setBuyerAssignedId(tender.get(NUMAR_CONTRACT))
                        .setSupplyType(tender.get(TIP_CONTRACT))
                        .addPublication(new ParsedPublication()
                                .setBuyerAssignedId(tender.get(NUMAR_ANUNT_ATRIBUIRE))
                                .setPublicationDate(tender.get(DATA_ANUNT_ATRIBUIRE))
                                .setSourceFormType("CONTRACT_AWARD")
                                .setIsIncluded(true)
                                .setSource(PublicationSources.RO_APA))
                        .addPublication(new ParsedPublication()
                                .setBuyerAssignedId(tender.get(NUMAR_ANUNT_PARTICIPARE))
                                .setPublicationDate(tender.get(DATA_ANUNT_PARTICIPARE))
                                .setFormType("CONTRACT_NOTICE")
                                .setIsIncluded(false)
                                .setSource(PublicationSources.RO_APA))
                        .addFunding(new ParsedFunding()
                                .setIsEuFund(tender.get(FOND_EUROPEAN))
                                .setSource(tender.get(MODALITATI_FINANTARE))
                                .setProgramme(tender.get(TIP_FINANTARE)))
                        .setDeposits(tender.get(DEPOZITE_GARANTII))
                        .setTitle(tender.get(TITLU_CONTRACT))
                        .setIsFrameworkAgreement(tender.get(TIP_INCHEIERE_CONTRACT))
                        .setIsElectronicAuction(tender.get(CU_LICITATIE_ELECTRONICA))
                        .addCpv(new ParsedCPV()
                                .setIsMain(Boolean.TRUE.toString())
                                .setCode(tender.get(CPV_CODE))));
            }

            return parsedTenders;
        } catch (IOException e) {
            logger.error("Crawling failed with exception {}", e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
    }
}
