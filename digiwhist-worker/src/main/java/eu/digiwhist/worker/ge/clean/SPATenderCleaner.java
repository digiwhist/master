package eu.digiwhist.worker.ge.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.DocumentType;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.DocumentPlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
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
 * Tender cleaner for Georgia.
 *
 * @author Marek Mikes
 */
public class SPATenderCleaner extends BaseDigiwhistTenderCleaner {
    private static final String VERSION = "1";

    private static final Locale LOCALE = new Locale("ka");

    private static final NumberFormat NUMBER_FORMAT;
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator('.');
        formatSymbols.setGroupingSeparator('`');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy", LOCALE);

    private static final DateTimeFormatter DATETIME_FORMATTER = new DateTimeFormatterBuilder()
            .appendPattern("dd.MM.yyyy HH:mm")
            .toFormatter(LOCALE);

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender,
                                                               final CleanTender cleanTender) {
        return cleanTender;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        Map<String, Map<Enum, List<String>>> lotMappings = new HashMap<>();
        lotMappings.put("statusMapping", getLotStatusMapping());
        lotMappings.put("countryMapping", getCountryMapping());

        pluginRegistry
            .registerPlugin("date", new DatePlugin(DATE_FORMATTER))
            .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTER))
            .registerPlugin("bodies", new BodyPlugin(getBuyerTypeMapping(), null, getCountryMapping()))
            .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTER, lotMappings))
            .registerPlugin("publications",
                    new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTER, getFormTypeMapping()))
            .registerPlugin("procedureType", new TenderProcedureTypePlugin(getProcedureTypeMapping(), null))
            .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
            .registerPlugin("documents", new DocumentPlugin(NUMBER_FORMAT, DATETIME_FORMATTER,
                    getDocumentTypeMapping()))
            .registerPlugin("integer", new IntegerPlugin(NUMBER_FORMAT));
    }

    /**
     * @return country mapping mapping
     */
    private Map<Enum, List<String>> getCountryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(CountryCode.GE, Arrays.asList("Georgia"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> getFormTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("Tender announced", "Bidding commenced",
                "Additional rounds of trade ended", "Selection/Evaluation", "Winner identified",
                "Finalization of contract"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("Contract awarded"));
        mapping.put(PublicationFormType.CONTRACT_CANCELLATION, Arrays.asList("No bids received", "Cancelled",
                "Contract not awarded"));

        return mapping;
    }

    /**
     * @return procedure type mapping for cleaning process
     */
    private static Map<Enum, List<String>> getProcedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList(
                "Consolidated Tender(CON)",
                "Contest (CNT)",
                "Donor electronic procurement procedure(DEP)",
                "Electronic Tender (DAP)",
                "Electronic Tender (SPA)",
                "Electronic Tender (SPA) via price list",
                "Electronic Tender Without Reverse Auction(NAT)",
                "Electronic Tender Without Reverse Auction(NAT) via price list",
                "e-Procurement Procedure(GEO) via price list",
                "e-Procurement Procedure(GEO)",
                "Simplified Electronic Tender(DAP)",
                "Simplified Electronic Tender(SPA)",
                "Simplified Electronic Tender(SPA) via price list",
                "Simplified Electronic Tender Without Reverse Auction(NAT)",
                "Simplified Electronic Tender Without Reverse Auction(NAT) via price list",
                "Simplified Two Stage Electronic Tender(MEP)",
                "Two Stage Electronic Tender(MEP)",
                "Two Stage Electronic Tender(MEP) via price list"));

        return mapping;
    }

    /**
     * @return lot status mapping for cleaning process
     */
    private static Map<Enum, List<String>> getLotStatusMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderLotStatus.AWARDED, Arrays.asList("Contract awarded", "Winner identified"));
        mapping.put(TenderLotStatus.CANCELLED, Arrays.asList("No bids received", "Contract not awarded", "Cancelled"));
        mapping.put(TenderLotStatus.ANNOUNCED, Arrays.asList("Finalization of contract", "Tender announced",
                "Bidding completed", "Selection/Evaluation", "Bidding commenced"));

        return mapping;
    }

    /**
     * @return buyer type mapping
     */
    private static Map<Enum, List<String>> getBuyerTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("Legal Entity of Public Law",
                "Non-entrepreneurial Non-commercial Legal Entity"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList("State Authority"));
        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("Local Self-Government Authority"));

        return mapping;
    }

    /**
     * @return document type mapping
     */
    private Map<Enum, List<String>> getDocumentTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(DocumentType.CONTRACTOR_AGREEMENT, Arrays.asList(DocumentType.CONTRACTOR_AGREEMENT.name()));

        return mapping;
    }

}
