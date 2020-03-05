package eu.datlab.worker.sk.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.utils.CodeTableUtils;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.Locale;

/**
 * Cleans EKS tenders.
 *
 * @author Tomas Mrazek
 */
public final class EKSTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final Locale LOCALE = new Locale("sk");

    private static final NumberFormat NUMBER_FORMAT;
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator('.');
        formatSymbols.setGroupingSeparator(' ');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss");

    @Override
    protected void registerSpecificPlugins() {
        pluginRegistry
            .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMAT))
            .registerPlugin("date", new DatePlugin(DATE_FORMATTER))
            .registerPlugin("bodies", new BodyPlugin(Collections.EMPTY_MAP, Collections.EMPTY_MAP))
            .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTER, Collections.EMPTY_MAP))
            .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
            .registerPlugin("publications", new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTER,
                CodeTableUtils.enumToMapping(PublicationFormType.class)));
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected CleanTender postProcessSourceSpecificRules(final ParsedTender parsedItem, final CleanTender cleanItem) {
        return cleanItem;
    }

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }
}
