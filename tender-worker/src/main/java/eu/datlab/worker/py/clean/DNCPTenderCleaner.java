package eu.datlab.worker.py.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.datlab.worker.py.DNCPFormType;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.UnitType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.CorrigendumPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.DocumentPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Cleaner for Paraguay.
 */
public class DNCPTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final Locale LOCALE = new Locale("py");

    private static final NumberFormat NUMBER_FORMAT;
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator('.');
        formatSymbols.setGroupingSeparator(' ');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ISO_DATE,
            DateTimeFormatter.ISO_DATE_TIME,
            DateTimeFormatter.ofPattern("uuuu-MM-dd hh:mm:ss"),
            DateTimeFormatter.ISO_INSTANT);

    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
        DateTimeFormatter.ISO_DATE_TIME,
        DateTimeFormatter.ofPattern("uuuu-MM-dd hh:mm:ss"),
        DateTimeFormatter.ISO_INSTANT
    );

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        Map<String, Map<Enum, List<String>>> lotMappings = new HashMap<>();
        lotMappings.put("unitPriceMapping", unitTypeMapping());
        lotMappings.put("countryMapping", countryMapping());

        pluginRegistry
            .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTERS, lotMappings))
            .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
            .registerPlugin("bodies", new BodyPlugin(null, null, null))
            .registerPlugin("publications", new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTERS, formTypeMapping()))
            .registerPlugin("date", new DatePlugin(DATE_FORMATTERS))
            .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTERS))
            .registerPlugin("price", new PricePlugin(NUMBER_FORMAT))
            .registerPlugin("documents", new DocumentPlugin(NUMBER_FORMAT, DATE_FORMATTERS, null))
            .registerPlugin("corrigendum", new CorrigendumPlugin(NUMBER_FORMAT, DATE_FORMATTERS));
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList(DNCPFormType.AWARD.name(), DNCPFormType.CONTRACT.name()));
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList(DNCPFormType.TENDER.name()));
        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList(DNCPFormType.PLANNING.name()));
        mapping.put(PublicationFormType.CONTRACT_AMENDMENT, Arrays.asList("Renovación de Alquiler de Inmueble", "Ampliación de Monto",
            "Ampliación de Plazo", "Otras Modificaciones", "Reajuste de Precio", DNCPFormType.AMENDMENT.name()));

        return mapping;
    }

    /**
     * @return country mapping
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(CountryCode.PY, Arrays.asList("Paraguay"));
        mapping.put(CountryCode.MX, Arrays.asList("México"));
        mapping.put(CountryCode.CN, Arrays.asList("China"));
        mapping.put(CountryCode.BO, Arrays.asList("Bolivia"));
        mapping.put(CountryCode.BR, Arrays.asList("Brasil"));
        mapping.put(CountryCode.US, Arrays.asList("Estados Unidos"));
        mapping.put(CountryCode.ES, Arrays.asList("España"));
        mapping.put(CountryCode.CO, Arrays.asList("Colombia"));
        mapping.put(CountryCode.AR, Arrays.asList("Argentina"));
        mapping.put(CountryCode.FR, Arrays.asList("FRANCIA"));
        mapping.put(CountryCode.IL, Arrays.asList("ISRAEL"));
        mapping.put(CountryCode.PT, Arrays.asList("Portugal"));
        mapping.put(CountryCode.CA, Arrays.asList("Canadá"));
        mapping.put(CountryCode.IN, Arrays.asList("India"));
        mapping.put(CountryCode.SI, Arrays.asList("Eslovenia"));
        mapping.put(CountryCode.CL, Arrays.asList("Chile"));

        return mapping;
    }

    /**
     * @return unit type mapping
     */
    private Map<Enum, List<String>> unitTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(null, Arrays.asList("_NO_APLICA_", "null"));
        mapping.put(UnitType.OTHER, Arrays.asList("Determinación", "Costo por Mil", "Ración", "Unidad Medida Global",
            "Kilogramos s/ metro cuadrado", "Unidad Internacional"));
        mapping.put(UnitType.CUBIC_CENTIMETERS, Arrays.asList("Centimetros cubicos"));
        mapping.put(UnitType.YARDS, Arrays.asList("Yardas"));
        mapping.put(UnitType.GRAMS, Arrays.asList("Gramos"));
        mapping.put(UnitType.METERS, Arrays.asList("Metros"));
        mapping.put(UnitType.MONTHS, Arrays.asList("Mes"));
        mapping.put(UnitType.PIECES, Arrays.asList("Unidad"));
        mapping.put(UnitType.INCHES, Arrays.asList("Pulgadas"));
        mapping.put(UnitType.KILOMETERS, Arrays.asList("Kilómetros"));
        mapping.put(UnitType.DAYS, Arrays.asList("Día"));
        mapping.put(UnitType.TONS, Arrays.asList("Tonelada"));
        mapping.put(UnitType.SQUARE_METERS, Arrays.asList("Metros cuadrados"));
        mapping.put(UnitType.KILOGRAMS, Arrays.asList("Kilogramos"));
        mapping.put(UnitType.CUBIC_METERS, Arrays.asList("Metros cúbicos"));
        mapping.put(UnitType.SQUARE_CENTIMETERS, Arrays.asList("Centimetros cuadrados"));
        mapping.put(UnitType.METERS, Arrays.asList("Metro lineal"));
        mapping.put(UnitType.HOURS, Arrays.asList("Hora"));
        mapping.put(UnitType.HECTARES, Arrays.asList("Hectáreas"));
        mapping.put(UnitType.MILLILITRES, Arrays.asList("Mililitros"));
        mapping.put(UnitType.LITRES, Arrays.asList("Litros"));
        mapping.put(UnitType.YEARS, Arrays.asList("Año"));
        mapping.put(UnitType.MILLIGRAMS, Arrays.asList("Miligramos"));
        mapping.put(UnitType.SQUARE_MILLIMETERS, Arrays.asList("Milímetros cuadrados"));
        mapping.put(UnitType.CENTIMETERS, Arrays.asList("Centimetros"));
        mapping.put(UnitType.SECONDS, Arrays.asList("Segundo"));
        mapping.put(UnitType.MINUTES, Arrays.asList("Minuto"));

        return mapping;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedItem, final CleanTender cleanItem) {
        return cleanItem;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        // datetime string pre-processing.
        parsedItem.setEnquiryDeadline(fixDateTime(parsedItem.getEnquiryDeadline()));
        parsedItem.setBidDeadline(fixDateTime(parsedItem.getBidDeadline()));
        parsedItem.setAwardDeadline(fixDateTime(parsedItem.getAwardDeadline()));
        parsedItem.setContractSignatureDate(fixDateTime(parsedItem.getContractSignatureDate()));

        return parsedItem;
    }

    /**
     * Attempts to fix datetime string by the removing of second time data in case that it includes two time data
     * (eg. 2013-06-21T09:00:00-04:00) otherwise returns original string.
     *
     * @param datetime
     *      datetime string to be fixed
     * @return fixed datetime string or original string
     */
    private static String fixDateTime(final String datetime) {
        if (datetime == null) {
            return null;
        }

        Matcher m = Pattern.compile("(?<datetime>\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2})-\\d{2}:\\d{2}").matcher(datetime.trim());
        return m.find() ? m.group("datetime") : datetime;
    }
}
