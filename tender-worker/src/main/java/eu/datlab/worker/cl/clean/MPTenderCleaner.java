package eu.datlab.worker.cl.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanBid;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.clean.CleanTenderLot;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.generic.UnitPrice;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.CorrigendumPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.FundingsPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSizePlugin;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Tender cleaner.
 *
 * @author Michal Riha
 */
public class MPTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1";

    private static final Locale LOCALE = new Locale("cl");

    private static final List<NumberFormat> NUMBER_FORMATS = new ArrayList<>();
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator(',');
        formatSymbols.setGroupingSeparator('.');
        NUMBER_FORMATS.add(new DecimalFormat("#,##0.###", formatSymbols));
    }

    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
        new DateTimeFormatterBuilder()
            .appendPattern("d-M-yyyy[ H:m[:s]]")
            .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
            .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
            .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
            .toFormatter(LOCALE),
        new DateTimeFormatterBuilder()
            .appendPattern("d/M/yyyy[ H:m[:s]]")
            .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
            .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
            .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
            .toFormatter(LOCALE)
    );
    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        if (parsedItem.getEstimatedPrice() != null) {
            parsedItem.getEstimatedPrice().setCurrency(cleanCurrency(parsedItem.getEstimatedPrice().getCurrency()));
        }

        if (parsedItem.getFinalPrice() != null) {
            parsedItem.getFinalPrice().setCurrency(cleanCurrency(parsedItem.getFinalPrice().getCurrency()));
        }

        if (parsedItem.getLots() != null) {
            for (ParsedTenderLot lot : parsedItem.getLots()) {
                if (lot.getEstimatedPrice() != null) {
                    lot.getEstimatedPrice().setCurrency(cleanCurrency(lot.getEstimatedPrice().getCurrency()));
                }

                if (lot.getBids() != null) {
                    for (ParsedBid bid : lot.getBids()) {
                        if (bid.getPrice() != null) {
                            bid.getPrice().setCurrency(cleanCurrency(bid.getPrice().getCurrency()));
                        }

                        if (bid.getUnitPrices() != null && !bid.getUnitPrices().isEmpty()) {
                            bid.getUnitPrices().get(0).setCurrency(cleanCurrency(bid.getUnitPrices().get(0).getCurrency()));
                        }

                        if (bid.getIsWinning() != null) {
                            if (bid.getIsWinning().equals("Adjudicada")) {
                                bid.setIsWinning(Boolean.TRUE.toString());
                            } else if (bid.getIsWinning().equals("No Adjudicada")) {
                                bid.setIsWinning(Boolean.FALSE.toString());
                            }
                        }
                    }
                }
            }
        }

        return parsedItem;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender, final CleanTender cleanTender) {
        if (cleanTender.getLots() != null) {
            for (CleanTenderLot lot : cleanTender.getLots()) {
                if (lot.getBids() != null) {
                    for (CleanBid bid : lot.getBids()) {
                        if (bid.getPrice() == null && bid.getUnitPrices() != null && !bid.getUnitPrices().isEmpty()
                                && bid.getUnitPrices().get(0).getUnitNumber() != null
                                && bid.getUnitPrices().get(0).getNetAmount() != null) {
                            final UnitPrice unitPrice = bid.getUnitPrices().get(0);
                            bid.setPrice(new Price()
                                    .setNetAmount(unitPrice.getNetAmount().multiply(BigDecimal
                                            .valueOf(unitPrice.getUnitNumber())))
                                    .setCurrency(unitPrice.getCurrency())
                            );
                        }
                    }
                }
            }
        }
        return cleanTender;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        pluginRegistry
            .registerPlugin("date", new DatePlugin(DATETIME_FORMATTERS))
            .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTERS))
            .registerPlugin("bodies", new BodyPlugin(null, null, countryMapping()))
            .registerPlugin("address", new AddressPlugin())
            .registerPlugin("lots", new LotPlugin(NUMBER_FORMATS, DATETIME_FORMATTERS, new HashMap<>()))
            .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMATS))
            .registerPlugin("publications", new PublicationPlugin(NUMBER_FORMATS, DATETIME_FORMATTERS, formTypeMapping()))
            .registerPlugin("procedureType", new TenderProcedureTypePlugin(procedureTypeMapping(), null))
            .registerPlugin("prices", new PricePlugin(NUMBER_FORMATS))
            .registerPlugin("corrections", new CorrigendumPlugin(NUMBER_FORMATS, DATETIME_FORMATTERS))
            .registerPlugin("tenderSize", new TenderSizePlugin(null))
            .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMATS));
    }

    /**
     * @return procedure type mapping for cleaning process
     */
    private static Map<Enum, List<String>> procedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList("ABIERTO"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("CERRADO"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("Publicada", "Cerrada", "Fecha de Publicación"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("Adjudicada", "Autorizada para Adjudicación", "Enviada a Autorizar " +
                "para Adjudicación", "Readjudicada"));
        mapping.put(PublicationFormType.CONTRACT_CANCELLATION, Arrays.asList("Autorizada para Deserción", "Desierta (o art. 3 " +
                "ó 9 Ley 19.886)", "Enviada a Autorizar para Deserción"));
        mapping.put(PublicationFormType.OTHER, Arrays.asList("Suspendida", "Revocada", "Fecha de Cierre"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(CountryCode.CL, Arrays.asList("CL"));

        return mapping;
    }

    /**
     * @param currency currency
     *
     * @return currency
     */
    private String cleanCurrency(final String currency) {
        if (currency == null) {
            return null;
        }

        return currency
            .replace("US$", "USD")
            .replace("$", "CLP")
            .replace("€", "EUR")
            .replace("Peso Chileno", "CLP")
            .replace("Dólar Americano", "USD")
            .replace("Euro", "EUR")
            .replace("Unidad de Fomento", "CLF")
            .replace("UF", "CLF")
            .replace("Unidad Tributaria Mensual", "UTM");
    }
}
