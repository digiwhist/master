package eu.dl.worker.master.plugin.specific;

import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.utils.BasePlugin;
import org.apache.commons.lang3.tuple.Pair;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Currency;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * Plugin for digiwhist-price calculation on tender and winning bid level.
 *
 * @author Tomas Mrazek
 */
public class DigiwhistPricePlugin extends BasePlugin implements MasterPlugin<MatchedTender, MasterTender, MatchedTender> {

    @Override
    public final MasterTender master(final List<MatchedTender> items, final MasterTender finalItem, final List<MatchedTender> context) {
        if (finalItem == null) {
            return null;
        }
        // tender best price
        finalItem.setDigiwhistPrice(getTenderDigiwhistPrice(finalItem));

        //winning bid best prices
        Optional.ofNullable(finalItem.getLots()).orElse(Collections.emptyList()).forEach(l -> {
            Optional.ofNullable(l.getBids()).orElse(Collections.emptyList()).stream()
                .filter(b -> Boolean.TRUE.equals(b.getIsWinning()))
                .forEach(b -> b.setDigiwhistPrice(getBidDigiwhistPrice(finalItem, l, b)));
        });

        return finalItem;
    }

    /**
     * @param tender
     *      master tender
     * @return best price for tender or null
     */
    @SuppressWarnings("unchecked")
    private Price getTenderDigiwhistPrice(final MasterTender tender) {
        BigDecimal amount = getPriceAmount(tender,
            t -> getBestPrice(t.getFinalPrice()),
            // sum of prices of all wining bids
            t -> Optional.ofNullable(t.getLots()).orElse(Collections.emptyList()).stream()
                    .map(MasterTenderLot::getBids).filter(Objects::nonNull).flatMap(List::stream)
                    .filter(n -> Boolean.TRUE.equals(n.getIsWinning()))
                    .map(MasterBid::getPrice).filter(Objects::nonNull).map(DigiwhistPricePlugin::getBestPrice)
                    .reduce(BigDecimal.ZERO, BigDecimal::add),
            t -> getBestPrice(t.getEstimatedPrice()),
            // sum of lot estimated prices
            t -> Optional.ofNullable(t.getLots()).orElse(Collections.emptyList()).stream()
                    .map(MasterTenderLot::getEstimatedPrice).map(DigiwhistPricePlugin::getBestPrice)
                    .reduce(BigDecimal.ZERO, BigDecimal::add));

        return getPrice(amount);
    }

    /**
     * For the given item performs functions one by one (preserves the order of arguments) and returns the first not-null result.
     *
     * @param <T>
     *     item class
     * @param item
     *      item used as input parameter for functions
     * @param functions
     *      list of functions which calculate price amount
     * @return amount greater than zero or zero
     */
    @SuppressWarnings("unchecked")
    private static <T> BigDecimal getPriceAmount(final T item, final Function<T, BigDecimal>... functions) {
        if (item == null) {
            return BigDecimal.ZERO;
        }

        List<Pair<Object, Function<Object, BigDecimal>>> items = new ArrayList<>();
        for (Function<T, BigDecimal> f : functions) {
            items.add(Pair.of(item, (Function<Object, BigDecimal>) f));
        }


        return getPriceAmount(items.toArray(new Pair[0]));
    }

    /**
     * For each pair from items performs function (value of pair) with its argument (key of pair) and returns the first not-null result.
     * Preserves the order of arguments.
     *
     * @param items
     *      list of functions and arguments
     * @return amount greater than zero or zero
     */
    @SafeVarargs
    private static BigDecimal getPriceAmount(final Pair<Object, Function<Object, BigDecimal>>... items) {
        if (items == null) {
            return BigDecimal.ZERO;
        }

        for (Pair<Object, Function<Object, BigDecimal>> n : items) {
            BigDecimal amount = n.getValue().apply(n.getKey());
            if (amount != null && amount.compareTo(BigDecimal.ZERO) > 0) {
                return amount;
            }
        }

        return BigDecimal.ZERO;
    }

    /**
     * @param price
     *      price whose value should be returned
     * @return first non-null value of the given price or zero
     */
    private static BigDecimal getBestPrice(final Price price) {
        if (price == null) {
            return BigDecimal.ZERO;
        }

        // returns first non-null value
        return Stream.of(price.getNetAmountEur(), price.getMaxNetAmount(), price.getMinNetAmount())
            .filter(Objects::nonNull).findFirst().orElse(BigDecimal.ZERO);
    }

    /**
     * @param amount
     *      amount
     * @return price with netAmountEur if the given amount > 0 and not null, otherwise null
     */
    private static Price getPrice(final BigDecimal amount) {
        if (amount == null || amount.equals(BigDecimal.ZERO)) {
            return null;
        }

        return new Price()
            .setNetAmount(amount)
            .setNetAmountEur(amount)
            .setCurrency(Currency.getInstance("EUR"));
    }

    /**
     * @param tender
     *      master tender
     * @param lot
     *      processed lot
     * @param bid
     *      processed winning bid
     * @return bid digiwhist price
     */
    private Price getBidDigiwhistPrice(final MasterTender tender, final MasterTenderLot lot, final MasterBid bid) {
        if (tender.getLots().size() > 1) {
            return getMultipleLotPrice(tender, lot, bid);
        } else {
            return getSingleLotPrice(tender, lot, bid);
        }
    }

    /**
     * @param tender
     *      master tender
     * @param lot
     *      processed lot
     * @param bid
     *      processed winning bid
     * @return bid digiwhist price for tender with one lot
     */
    private Price getSingleLotPrice(final MasterTender tender, final MasterTenderLot lot, final MasterBid bid) {
        BigDecimal winningBidsCount = new BigDecimal(Optional.ofNullable(lot.getBids()).orElse(Collections.emptyList()).stream()
            .filter(n -> n != null && Boolean.TRUE.equals(n.getIsWinning())).count());

        BigDecimal amount = getPriceAmount(
            Pair.of(bid, b -> getBestPrice(((MasterBid) b).getPrice())),
            Pair.of(tender, t -> getBestPrice(((MasterTender) t).getFinalPrice())
                .divide(winningBidsCount, 6, RoundingMode.HALF_UP)),
            Pair.of(lot, l -> getBestPrice(((MasterTenderLot) l).getEstimatedPrice())
                .divide(winningBidsCount, 6, RoundingMode.HALF_UP)),
            Pair.of(tender, t -> getBestPrice(((MasterTender) t).getEstimatedPrice())
                .divide(winningBidsCount, 6, RoundingMode.HALF_UP)));

        return getPrice(amount);
    }

    /**
     * @param tender
     *      master tender
     * @param lot
     *      processed lot
     * @param bid
     *      processed winning bid
     * @return bid digiwhist price for tender with multiple lots
     */
    private Price getMultipleLotPrice(final MasterTender tender, final MasterTenderLot lot, final MasterBid bid) {
        BigDecimal winningBidsCount = new BigDecimal(Optional.ofNullable(lot.getBids()).orElse(Collections.emptyList()).stream()
            .filter(n -> n != null && Boolean.TRUE.equals(n.getIsWinning())).count());

        BigDecimal amount = getPriceAmount(
            Pair.of(bid, b -> getBestPrice(((MasterBid) b).getPrice())),
            Pair.of(lot, l -> getBestPrice(((MasterTenderLot) l).getEstimatedPrice())
                .divide(winningBidsCount, 6, RoundingMode.HALF_UP)));

        return getPrice(amount);
    }
}