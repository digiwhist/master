package eu.datlab.worker.sk.clean;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.clean.plugin.BaseCleaningPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;

import static eu.datlab.worker.sk.clean.UvoTenderCleanerUtils.cleanUvoPrice;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;

/**
 * Tender clean lot plugin plugin specific for SK Uvo.
 *
 * @author Michal Riha
 */
public class UvoTenderLotPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {
    private NumberFormat numberFormat;
    private List<DateTimeFormatter> formatters;
    private Map<String, Map<Enum, List<String>>> lotMappings;

    /**
     * UvoTenderLotPlugin should be initialised with local and pattern of the date.
     */
    protected UvoTenderLotPlugin() {
    }

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *         number format
     * @param formatters
     *         list of datetime formatters
     * @param lotMappings
     *         lot mappings
     */
    public UvoTenderLotPlugin(
            final NumberFormat numberFormat,
            final List<DateTimeFormatter> formatters,
            final Map<String, Map<Enum, List<String>>> lotMappings
    ) {
        this.numberFormat = numberFormat;
        this.formatters = formatters;
        this.lotMappings = lotMappings;
    }

    @Override
    public final CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        if (parsedTender.getLots() != null) {
            for (ParsedTenderLot parsedLot : parsedTender.getLots()) {
                if (parsedLot.getEstimatedPrice() != null) {
                    parsedLot.getEstimatedPrice().setNetAmount(cleanUvoPrice(parsedLot.getEstimatedPrice()
                            .getNetAmount()));
                    parsedLot.getEstimatedPrice().setAmountWithVat(cleanUvoPrice(parsedLot.getEstimatedPrice()
                            .getAmountWithVat()));
                }

                if (parsedLot.getBids() != null) {
                    for (ParsedBid parsedBid : parsedLot.getBids()) {
                        if (parsedBid.getPrice() != null) {
                            parsedBid.getPrice().setNetAmount(cleanUvoPrice(parsedBid.getPrice().getNetAmount()));
                            parsedBid.getPrice().setAmountWithVat(cleanUvoPrice(parsedBid.getPrice()
                                    .getAmountWithVat()));
                            parsedBid.getPrice().setMinNetAmount(cleanUvoPrice(parsedBid.getPrice().getMinNetAmount()));
                            parsedBid.getPrice().setMaxNetAmount(cleanUvoPrice(parsedBid.getPrice().getMaxNetAmount()));
                        }
                    }
                }
            }
        }

        return new LotPlugin(numberFormat, formatters, lotMappings).clean(parsedTender, cleanTender);
    }


}
