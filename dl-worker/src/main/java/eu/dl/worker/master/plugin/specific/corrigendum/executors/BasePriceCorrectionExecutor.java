package eu.dl.worker.master.plugin.specific.corrigendum.executors;

import eu.dl.dataaccess.dto.generic.Corrigendum;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.master.MasterTender;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

/**
 * Base correction executor which applies corrections of price fields.
 *
 * @param <C>
 *     context class of price
 */
public abstract class BasePriceCorrectionExecutor<C> extends BaseCorrectionExecutor<Price> {

    /**
     * Price comparison tolerance as maximal difference between two prices in percentage. Compared prices may not be the same to consider
     * them as same.
     */
    protected static final double COMPARISON_TOLERANCE = 0.1d;

    protected boolean amountWithVatCorrection = false;

    @Override
    public final void execute(final Corrigendum c, final MasterTender t) {
        // get origin/replacement value of an requested data type
        Price origin = getCorrectionOrigin(c);
        Price replacement = getCorrectionReplacement(c);
        // no replacement found, skip
        if (replacement == null) {
            return;
        }

        boolean isSingleLotAndBid = t.getLots() != null && t.getLots().size() == 1
            && t.getLots().get(0).getBids() != null && t.getLots().get(0).getBids().size() == 1;

        List<Pair<Price, C>> prices = getTenderOriginPrices(t);
        boolean included = false;
        for (Pair<Price, C> p : prices) {
            Price price = p.getKey();
            C context = p.getValue();
            if (price == null || origin == null) {
                if (isSingleLotAndBid && context != null && applyReplacementOnNull(context, replacement)) {
                    included = true;
                }
            } else if (isEqual(price, origin) || isEqualToIncludedCorrection(t, origin)) {
                applyReplacement(price, replacement);
                included = true;
            }
        }

        if (included) {
            c.setIsIncluded(true);
            c.setType(getCorrectionType());
        }

        if (!Boolean.TRUE.equals(c.getIsIncluded())) {
            logger.warn("The correction wasn't executed because of replaced value is not equal to correction's original value " +
                "and to any of previous values.");
            c.setIsIncluded(false);
        }
    }

    /**
     * @param t
     *      master tender
     * @return list of tender`s origin values or empty list
     */
    protected abstract List<Pair<Price, C>> getTenderOriginPrices(MasterTender t);

    /**
     * Applies replacement in case when the tender/lot/bid origin price is null.
     *
     * @param context
     *      price context
     * @param replacement
     *      replacement to be applied
     * @return TRUE if replacement was applied, otherwise FALSE
     */
    protected abstract boolean applyReplacementOnNull(C context, Price replacement);

    @Override
    protected final Price getCorrectionOrigin(final Corrigendum c) {
        return c.getOriginalValue();
    }

    @Override
    protected final Price getCorrectionReplacement(final Corrigendum c) {
        return c.getReplacementValue();
    }

    /**
     * Checks if prices are equal.
     *
     * @param o1
     *      price
     * @param o2
     *      price
     * @return true if prices are equal.
     */
    @Override
    protected final boolean isEqual(final Price o1, final Price o2) {
        if (ObjectUtils.allNotNull(o1.getNetAmount(), o2.getNetAmount())
            && amountDiff(o1.getNetAmount(), o2.getNetAmount()) <= COMPARISON_TOLERANCE) {
            amountWithVatCorrection = false;
            return true;
        }

        if (ObjectUtils.allNotNull(o1.getAmountWithVat(), o2.getAmountWithVat())
            && amountDiff(o1.getAmountWithVat(), o2.getAmountWithVat()) <= COMPARISON_TOLERANCE) {
            amountWithVatCorrection = true;
            return true;
        }

        return false;
    }

    /**
     * Returns amount difference in percent. If amount a and/or amount b is NULL return 100. In other cases
     * compares amounts. |diff(a, b)|/(max(a,b)/100)
     *
     * @param a
     *      first amount
     * @param b
     *      second amount
     * @return percentage difference of amounts.
     */
    private static double amountDiff(final BigDecimal a, final BigDecimal b) {
        if (a == null || b == null) {
            return 100;
        }

        return a.subtract(b).abs().divide(a.max(b)
            .divide(BigDecimal.valueOf(100), 6, RoundingMode.HALF_UP), 6, RoundingMode.HALF_UP).doubleValue();
    }

    /**
     * @param source
     *      source price to be corrected
     * @param replacement
     *      replacement to be applied
     */
    protected final void applyReplacement(final Price source, final Price replacement) {
        if (amountWithVatCorrection) {
            source
                .setAmountWithVat(replacement.getNetAmount())
                .setNetAmount(null);
        } else {
            source
                .setNetAmount(replacement.getNetAmount())
                .setAmountWithVat(null);
        }

        source
            .setCurrency(replacement.getCurrency())
            .setVat(replacement.getVat());

        if (replacement.getNetAmountEur() != null) {
            source.setNetAmountEur(replacement.getNetAmountEur());
        }
    }
}
