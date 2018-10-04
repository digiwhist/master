package eu.datlab.worker.sk.clean;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BaseCleaningPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;

import static eu.datlab.worker.sk.clean.UvoTenderCleanerUtils.cleanUvoPrice;

import java.text.NumberFormat;

/**
 * Tender clean price plugin specific for SK Uvo.
 *
 * @author Michal Riha
 */
public class UvoTenderPricePlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {
    /**
     * Number format.
     */
    private NumberFormat numberFormat;

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *         number format
     */
    UvoTenderPricePlugin(final NumberFormat numberFormat) {
        this.numberFormat = numberFormat;
    }

    @Override
    public final CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        if (parsedTender.getDocumentsPrice() != null) {
            parsedTender.getDocumentsPrice().setAmountWithVat(cleanUvoPrice(parsedTender.getDocumentsPrice()
                    .getAmountWithVat()));
            parsedTender.getDocumentsPrice().setNetAmount(cleanUvoPrice(parsedTender.getDocumentsPrice()
                    .getNetAmount()));
        }

        if (parsedTender.getEstimatedPrice() != null) {
            parsedTender.getEstimatedPrice().setAmountWithVat(cleanUvoPrice(parsedTender.getEstimatedPrice()
                    .getAmountWithVat()));
            parsedTender.getEstimatedPrice().setNetAmount(cleanUvoPrice(parsedTender.getEstimatedPrice()
                    .getNetAmount()));
            parsedTender.getEstimatedPrice().setMinNetAmount(cleanUvoPrice(parsedTender.getEstimatedPrice()
                    .getMinNetAmount()));
            parsedTender.getEstimatedPrice().setMaxNetAmount(cleanUvoPrice(parsedTender.getEstimatedPrice()
                    .getMaxNetAmount()));
            parsedTender.getEstimatedPrice().setMinAmountWithVat(cleanUvoPrice(parsedTender.getEstimatedPrice()
                    .getMinAmountWithVat()));
            parsedTender.getEstimatedPrice().setMaxAmountWithVat(cleanUvoPrice(parsedTender.getEstimatedPrice()
                    .getMaxAmountWithVat()));
        }

        if (parsedTender.getFinalPrice() != null) {
            parsedTender.getFinalPrice().setAmountWithVat(cleanUvoPrice(parsedTender.getFinalPrice()
                    .getAmountWithVat()));
            parsedTender.getFinalPrice().setNetAmount(cleanUvoPrice(parsedTender.getFinalPrice()
                    .getNetAmount()));
            parsedTender.getFinalPrice().setMinNetAmount(cleanUvoPrice(parsedTender.getFinalPrice()
                    .getMinNetAmount()));
            parsedTender.getFinalPrice().setMaxNetAmount(cleanUvoPrice(parsedTender.getFinalPrice()
                    .getMaxNetAmount()));
            parsedTender.getFinalPrice().setMinAmountWithVat(cleanUvoPrice(parsedTender.getFinalPrice()
                    .getMinAmountWithVat()));
            parsedTender.getFinalPrice().setMaxAmountWithVat(cleanUvoPrice(parsedTender.getFinalPrice()
                    .getMaxAmountWithVat()));
        }

        if (parsedTender.getFinalPrice() != null) {
            parsedTender.getFinalPrice().setAmountWithVat(cleanUvoPrice(parsedTender.getFinalPrice()
                    .getAmountWithVat()));
            parsedTender.getFinalPrice().setNetAmount(cleanUvoPrice(parsedTender.getFinalPrice().getNetAmount()));
        }

        return new PricePlugin(numberFormat).clean(parsedTender, cleanTender);
    }
}
