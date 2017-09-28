package eu.digiwhist.worker.pt.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Tender notice and contract cleaner for Portugal.
 *
 * @author Tomas Mrazek
 */
abstract class BaseBASECleaner extends BaseDigiwhistTenderCleaner {
    protected static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd-MM-uuuu");

    protected static final NumberFormat NUMBER_FORMAT;
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(new Locale("pt"));
        formatSymbols.setDecimalSeparator(',');
        formatSymbols.setGroupingSeparator('.');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    /**
     * @return tender supply type mapping
     */
    protected Map<Enum, List<String>> getSupplyTypeMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Empreitadas de obras públicas"));
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Aquisição de serviços"));
        mapping.put(TenderSupplyType.OTHER, Arrays.asList("Concessão de obras públicas",
            "Concessão de serviços públicos", "Locação de bens móveis", "Sociedade", "Outros",
            "Aquisição de bens móveis"));

        return mapping;
    }
}
