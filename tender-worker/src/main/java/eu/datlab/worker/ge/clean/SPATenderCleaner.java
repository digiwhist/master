package eu.datlab.worker.ge.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
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
public class SPATenderCleaner extends BaseDatlabTenderCleaner {
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

        mapping.put(CountryCode.AE, Arrays.asList("United Arab Emirate"));
        mapping.put(CountryCode.AM, Arrays.asList("Armenia"));
        mapping.put(CountryCode.AT, Arrays.asList("Austria"));
        mapping.put(CountryCode.AU, Arrays.asList("Australia"));
        mapping.put(CountryCode.AZ, Arrays.asList("Azberbaijan"));
        mapping.put(CountryCode.BE, Arrays.asList("Belgium"));
        mapping.put(CountryCode.BG, Arrays.asList("Bulgaria"));
        mapping.put(CountryCode.BH, Arrays.asList("Bahrain"));
        mapping.put(CountryCode.BY, Arrays.asList("Belarus"));
        mapping.put(CountryCode.CA, Arrays.asList("Canada"));
        mapping.put(CountryCode.CH, Arrays.asList("Switzerland"));
        mapping.put(CountryCode.CN, Arrays.asList("China"));
        mapping.put(CountryCode.CY, Arrays.asList("Cyprus"));
        mapping.put(CountryCode.CZ, Arrays.asList("Czech Republic"));
        mapping.put(CountryCode.DE, Arrays.asList("Germany"));
        mapping.put(CountryCode.DK, Arrays.asList("Denmark"));
        mapping.put(CountryCode.EE, Arrays.asList("Estonia"));
        mapping.put(CountryCode.ES, Arrays.asList("Spain"));
        mapping.put(CountryCode.FI, Arrays.asList("Finland"));
        mapping.put(CountryCode.FR, Arrays.asList("France"));
        mapping.put(CountryCode.GB, Arrays.asList("United Kingdom"));
        mapping.put(CountryCode.GE, Arrays.asList("Georgia"));
        mapping.put(CountryCode.GR, Arrays.asList("Greece"));
        mapping.put(CountryCode.HK, Arrays.asList("Hong Kong"));
        mapping.put(CountryCode.HR, Arrays.asList("Croatia"));
        mapping.put(CountryCode.HU, Arrays.asList("Hungary"));
        mapping.put(CountryCode.IL, Arrays.asList("Israel"));
        mapping.put(CountryCode.IN, Arrays.asList("India"));
        mapping.put(CountryCode.IR, Arrays.asList("Iran"));
        mapping.put(CountryCode.IT, Arrays.asList("Italy"));
        mapping.put(CountryCode.KG, Arrays.asList("Kyrgyzstan"));
        mapping.put(CountryCode.KR, Arrays.asList("Korea, North"));
        mapping.put(CountryCode.KZ, Arrays.asList("Kazakhstan"));
        mapping.put(CountryCode.LT, Arrays.asList("Lithuania"));
        mapping.put(CountryCode.LV, Arrays.asList("Latvia"));
        mapping.put(CountryCode.MK, Arrays.asList("Macedonia"));
        mapping.put(CountryCode.NL, Arrays.asList("Netherlands"));
        mapping.put(CountryCode.NO, Arrays.asList("Norway"));
        mapping.put(CountryCode.PL, Arrays.asList("Poland"));
        mapping.put(CountryCode.PT, Arrays.asList("Portugal"));
        mapping.put(CountryCode.RU, Arrays.asList("Russia"));
        mapping.put(CountryCode.SE, Arrays.asList("Sweden"));
        mapping.put(CountryCode.SI, Arrays.asList("Slovenia"));
        mapping.put(CountryCode.SZ, Arrays.asList("Swaziland"));
        mapping.put(CountryCode.TR, Arrays.asList("Turkey"));
        mapping.put(CountryCode.TZ, Arrays.asList("Tanzania"));
        mapping.put(CountryCode.UA, Arrays.asList("Ukraine"));
        mapping.put(CountryCode.US, Arrays.asList("USA"));
        mapping.put(CountryCode.VG, Arrays.asList("British Virgin Isla"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> getFormTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("Tender announced", "Bidding commenced",
                "Additional rounds of trade ended", "Selection/Evaluation"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("Contract awarded", "Winner identified",
                "Finalization of contract"));
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
