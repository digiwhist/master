package eu.datlab.worker.za.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Cleaner.
 */
public class ETendersTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance(new Locale("gb"));
    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ISO_LOCAL_DATE_TIME, DateTimeFormatter.ISO_OFFSET_DATE_TIME);

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        Map<String, Map<Enum, List<String>>> lotMappings = new HashMap<>();

        pluginRegistry
                .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMAT))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))
                .registerPlugin("procedureType", new TenderProcedureTypePlugin(null, Arrays.asList("Accelerated",
                        "AcceleratedNegotiated", "OpenAcceleratedOJEU", "CompetitiveProcedureWithNegotiationAcceleratedOJEU",
                        "AcceleratedRestricted")))
                .registerPlugin("date", new DatePlugin<>(DATETIME_FORMATTERS))
                .registerPlugin("datetime", new DateTimePlugin<>(DATETIME_FORMATTERS))
                .registerPlugin("bodies", new BodyPlugin(null, null, null))
                .registerPlugin("publications", new PublicationPlugin(NUMBER_FORMAT, DATETIME_FORMATTERS, getFormTypeMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATETIME_FORMATTERS, lotMappings))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("documents", new DocumentPlugin(NUMBER_FORMAT, DATETIME_FORMATTERS, null))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("funding", new FundingsPlugin(NUMBER_FORMAT));
    }

    /**
     * @return supply type mapping
     */
    private Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Services"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Works"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Supplies"));
        mapping.put(TenderSupplyType.OTHER, Arrays.asList("Other"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> getFormTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("advertisedTender"));
        mapping.put(PublicationFormType.CONTRACT_CANCELLATION, Arrays.asList("canceledTender"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("awardedTender"));

        return mapping;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender,
                                                               final CleanTender cleanTender) {
        return cleanTender;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        if (parsedItem.getSupplyType() != null) {
            if (parsedItem.getSupplyType().toLowerCase().contains("services")) {
                parsedItem.setSupplyType("Services");
            } else if (parsedItem.getSupplyType().toLowerCase().contains("construction")) {
                parsedItem.setSupplyType("Works");
            } else if (parsedItem.getSupplyType().toLowerCase().contains("supplies")) {
                parsedItem.setSupplyType("Supplies");
            } else {
                parsedItem.setSupplyType("Other");
            }
        }

        if (parsedItem.getIsAwarded() != null) {
            if (parsedItem.getIsAwarded().contains("uccessful")) {
                parsedItem.setIsAwarded(Boolean.TRUE.toString());
            } else {
                parsedItem.setIsAwarded(Boolean.FALSE.toString());
            }
        }

        if (parsedItem.getLots() != null) {
            for (ParsedTenderLot lot : parsedItem.getLots()) {
                if (lot.getBids() != null) {
                    for (ParsedBid bid : lot.getBids()) {
                        if (bid.getPrice() != null && bid.getPrice().getNetAmount() != null) {
                            bid.getPrice().setNetAmount(bid.getPrice().getNetAmount().replaceAll("Contract Value: R", ""));
                        }
                    }
                }
            }
        }

        return parsedItem;
    }
}
