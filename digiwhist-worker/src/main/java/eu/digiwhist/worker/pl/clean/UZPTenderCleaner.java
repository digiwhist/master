package eu.digiwhist.worker.pl.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.clean.CleanTenderLot;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.CorrigendumPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.DocumentPlugin;
import eu.dl.worker.clean.plugin.FundingsPlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.SignStyle;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Cleaner for the polish xml forms.
 *
 * @author Tomas Mrazek
 */
public final class UZPTenderCleaner extends BaseDigiwhistTenderCleaner {
    private static final String VERSION = "1.0";

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance(new Locale("pl"));

    private static final List<DateTimeFormatter> DATE_FORMATTER;
    static {
        DATE_FORMATTER = new ArrayList<>();
        DATE_FORMATTER.add(DateTimeFormatter.ofPattern("dd/MM/yyyy"));
        DATE_FORMATTER.add(DateTimeFormatter.ofPattern("yyyy/MM/dd"));
    }

    /**
     * This DateTimeFormatter parses following datetime strings.
     * - yyyy-MM-dd (time is omitted)
     * - yyyy-MM-dd H:mm (hour as one digit)
     * - yyyy-MM-dd HH:mm (hour as two digits)
     */
    private static final DateTimeFormatter DATETIME_FORMATTER =
            new DateTimeFormatterBuilder().appendPattern("dd/MM/yyyy")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(":")
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter();

    /**
     * This DateTimeFormatter parses following date strings.
     * - yyyy-MM-dd
     * - yyyy (month and day ommited) - month=1, day=1
     */
    private static final DateTimeFormatter PUBLUCATIONS_DATE_FORMATTERS = new DateTimeFormatterBuilder()
            .appendPattern("yyyy")
            //optional month with day
            .optionalStart()
            .appendLiteral('-')
            .appendValue(ChronoField.MONTH_OF_YEAR, 2)
            .appendLiteral('-')
            .appendValue(ChronoField.DAY_OF_MONTH, 2)
            .optionalEnd()
            //default values for month and day
            .parseDefaulting(ChronoField.MONTH_OF_YEAR, 1)
            .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
            .toFormatter();

    @Override
    protected void registerSpecificPlugins() {
        pluginRegistry
                .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMAT))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))
                .registerPlugin("procedureType", new TenderProcedureTypePlugin(procedureTypeMapping()))
                .registerPlugin("date", new DatePlugin(DATE_FORMATTER))
                .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTER))
                .registerPlugin("bodies", new BodyPlugin(bodyTypeMapping(), null))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTER, formTypeMapping())
                            .addFormatter(PUBLUCATIONS_DATE_FORMATTERS))
                //document type isn't provided -> mapping = null
                //selection method isn't provided -> mapping = null
                //status isn't provided -> mapping = null
                //price unit isn't provided -> mapping = null
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTER, new HashMap<>()))
                //document type isn't provided -> mapping = null
                .registerPlugin("documents", new DocumentPlugin(NUMBER_FORMAT, DATE_FORMATTER, null))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMAT))
                .registerPlugin("corrections", new CorrigendumPlugin(NUMBER_FORMAT, DATE_FORMATTER));
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender,
                                                         final CleanTender cleanTender) {

        if (parsedTender.getProcedureType() != null && parsedTender.getProcedureType().equals("LE")) {
            cleanTender.setIsElectronicAuction(true);
        }

        // parsed information in validBidsCount indicates the number of excluded bids, not the number of valid bids.
        // We thus need to subtract this value from the value in bidsCount to obtain the correct value for
        // validBidsCount.
        if (cleanTender.getLots() != null) {
            for (CleanTenderLot lot : cleanTender.getLots()) {
                if (lot.getBidsCount() != null && lot.getValidBidsCount() != null) {
                    lot.setValidBidsCount(lot.getBidsCount() - lot.getValidBidsCount());
                }
            }
        }

        return cleanTender;
    }

    /**
     * @return supply type mapping for cleaning process
     */
    private static Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        //usługi
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("U"));
        //roboty budowlane
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("B"));
        //dostawy
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("D"));

        return mapping;
    }

    /**
     * @return procedure type mapping for cleaning process
     */
    private static Map<Enum, List<String>> procedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderProcedureType.OPEN, Arrays.asList("PN", "LE"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("PO"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("DK"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList("NO"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("NB", "WR"));
        mapping.put(TenderProcedureType.APPROACHING_BIDDERS, Arrays.asList("ZC"));
        mapping.put(TenderProcedureType.OTHER, Arrays.asList("KU"));

        return mapping;
    }

    /**
     * @return body type mapping
     */
    private static Map<Enum, List<String>> bodyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("Podmiot prawa publicznego", "Uczelnia publiczna"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList("Administracja rządowa centralna"));
        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("Administracja samorządowa",
                "Samodzielny publiczny zakład opieki zdrowotnej", "Administracja rządowa terenowa"));
        mapping.put(BuyerType.OTHER, Arrays.asList("Samodzielny publiczny zakład opieki zdrowotnej",
                "Instytucja ubezpieczenia społecznego i zdrowotnego",
                "Organ kontroli państwowej lub ochrony prawa, sąd lub trybunał"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("ZP-400", "ZP-401", "ZP-402", "ZP-404",
            "ZP-407", "ZP-409", "0", "1", "2", "4", "7", "9"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("ZP-403", "ZP-405", "ZP-408", "3", "5", "8"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList("ZP-406", "ZP-SPR", "6"));

        return mapping;
    }

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }
}
