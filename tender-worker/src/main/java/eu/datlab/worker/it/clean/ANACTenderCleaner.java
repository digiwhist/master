package eu.datlab.worker.it.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.BooleanPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Cleaner for IT data.
 */
public final class ANACTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final Locale LOCALE = new Locale("en");

    private static final NumberFormat NUMBER_FORMAT;

    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator('.');
        formatSymbols.setGroupingSeparator(',');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    private static final DateTimeFormatter DATETIME_FORMATTER = new DateTimeFormatterBuilder()
            .appendPattern("yyyy-MM-dd[+hh:mm]")
            //default values for time
            .toFormatter();

    @Override
    protected void registerSpecificPlugins() {
        pluginRegistry
                .registerPlugin("bodies", new BodyPlugin(null, null))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMAT, DATETIME_FORMATTER, null))
                .registerPlugin("lots",
                        new LotPlugin(NUMBER_FORMAT, DATETIME_FORMATTER, null))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTER))
                .registerPlugin("date", new DatePlugin(DATETIME_FORMATTER))
                .registerPlugin("boolean", new BooleanPlugin())
                .registerPlugin("procedureType", new TenderProcedureTypePlugin(procedureTypeMapping()));
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender,
                                                         final CleanTender cleanTender) {
        return cleanTender;
    }

    /**
     * @return supply type mapping for cleaning process
     */
    private static Map<Enum, List<String>> procedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderProcedureType.OPEN, Arrays.asList(
                "01-PROCEDURA APERTA"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList(
                "02-PROCEDURA RISTRETTA"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList(
                "03-PROCEDURA NEGOZIATA PREVIA PUBBLICAZIONE",
                "03-PROCEDURA NEGOZIATA PREVIA PUBBLICAZIONE DEL BANDO",
                "21-PROCEDURA RISTRETTA DERIVANTE DA AVVISI CON CUI SI INDICE LA GARA",
                "22-PROCEDURA NEGOZIATA CON PREVIA INDIZIONE DI GARA (SETTORI SPECIALI)",
                "22-PROCEDURA NEGOZIATA DERIVANTE DA AVVISI CON CUI SI INDICE LA GARA"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList(
                "04-PROCEDURA NEGOZIATA SENZA PREVIA PUBBLICAZIONE",
                "04-PROCEDURA NEGOZIATA SENZA PREVIA PUBBLICAZIONE DEL BANDO",
                "06-PROCEDURA NEGOZIATA SENZA PREVIA INDIZIONE DI GARA (SETTORI SPECIALI)",
                "06-PROCEDURA NEGOZIATA SENZA PREVIA INDIZIONE DI  GARA ART. 221 D.LGS. 163/2006"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList(
                "05-DIALOGO COMPETITIVO"));
        mapping.put(TenderProcedureType.DPS_PURCHASE, Arrays.asList(
                "07-SISTEMA DINAMICO DI ACQUISIZIONE"));
        mapping.put(TenderProcedureType.OTHER, Arrays.asList(
                "08-AFFIDAMENTO IN ECONOMIA - COTTIMO FIDUCIARIO",
                "17-AFFIDAMENTO DIRETTO EX ART. 5 DELLA LEGGE 381/91",
                "17-AFFIDAMENTO DIRETTO EX ART. 5 DELLA LEGGE N.381/91",
                "25-AFFIDAMENTO DIRETTO A SOCIETA' RAGGRUPPATE/CONSORZIATE O CONTROLLATE NELLE CONCESSIONI E NEI PARTENARIATI",
                "25-AFFIDAMENTO DIRETTO A SOCIETA' RAGGRUPPATE/CONSORZIATE O CONTROLLATE NELLE CONCESSIONI DI LL.PP",
                "28-PROCEDURA AI SENSI DEI REGOLAMENTI DEGLI ORGANI COSTITUZIONALI",
                "30-PROCEDURA DERIVANTE DA LEGGE REGIONALE",
                "34-PROCEDURA ART.16 COMMA 2-BIS DPR 380/2001 PER OPERE URBANIZZAZIONE A SCOMPUTO PRIMARIE SOTTO " +
                        "SOGLIA COMUNITARIA",
                "ART.36 DLGS 50/16 C.2 LETT.A            ", "ND",
                "34-PROCEDURA ART.16 COMMA 2-BIS DPR 380/2001 PER OPERE URBANIZZAZIONE A SCOMPUTO PRIMARIE SOTTO SOGLIA COMUNITARIA",
                "38-PROCEDURA DISCIPLINATA DA REGOLAMENTO INTERNO PER SETTORI SPECIALI"));
        mapping.put(TenderProcedureType.APPROACHING_BIDDERS, Arrays.asList(
                "14-PROCEDURA SELETTIVA EX ART 238 C.7, D.LGS. 163/2006",
                "29-PROCEDURA RISTRETTA SEMPLIFICATA"));
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList(
                "23-AFFIDAMENTO DIRETTO",
                "23-AFFIDAMENTO IN ECONOMIA - AFFIDAMENTO DIRETTO",
                "23 - AFFIDAMENTO IN ECONOMIA - AFFIDAMENTO DIRETTO",
                " 23 - AFFIDAMENTO IN ECONOMIA - AFFI IDAMENTO DIRETTO ",
                "AFFIDAMENTO DIRETTO", "affidamento diretto",
                "24-AFFIDAMENTO DIRETTO A SOCIETA' IN HOUSE",
                "31-AFFIDAMENTO DIRETTO PER VARIANTE SUPERIORE AL 20% DELL'IMPORTO CONTRATTUALE",
                "32-AFFIDAMENTO RISERVATO",
                "36-AFFIDAMENTO DIRETTO PER LAVORI, SERVIZI O FORNITURE SUPPLEMENTARI"));
        mapping.put(TenderProcedureType.MINITENDER, Arrays.asList(
                "26-AFFIDAMENTO DIRETTO IN ADESIONE AD ACCORDO QUADRO/CONVENZIONE",
                "27-CONFRONTO COMPETITIVO IN ADESIONE AD ACCORDO QUADRO/CONVENZIONE"));
        mapping.put(TenderProcedureType.NEGOTIATED, Arrays.asList(
                "33-PROCEDURA NEGOZIATA PER AFFIDAMENTI SOTTO SOGLIA",
                "37-PROCEDURA COMPETITIVA CON NEGOZIAZIONE"));
        mapping.put(TenderProcedureType.INOVATION_PARTNERSHIP, Arrays.asList(
                "35-PARTERNARIATO PER L'INNOVAZIONE"));

        return mapping;
    }

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }
}
