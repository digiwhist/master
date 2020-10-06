package eu.datlab.worker.master.plugin;

import eu.datlab.dataaccess.dao.jdbc.JdbcMasterTenderIndicatorDAO;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.worker.indicator.plugin.LotIndicatorPlugin;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Winner contract share indicator plugin.
 */
public class WinnerCAShareIndicatorPlugin extends LotIndicatorPlugin {

    /**
     * Total values per year.
     */
    public static final class TotalValuesPerYear {
        /**
         * Total values per year.
         */
        private final Map<Integer, Double> valuesPerYear;

        /**
         * Constructor.
         */
        public TotalValuesPerYear() {
            valuesPerYear = new HashMap<>();
        }

        /**
         * Getter for total value per given year.
         *
         * @param year year to get total value for
         * @return total value for given year or null if there are no data for given year
         */
        public Double getTotalValueForYear(final Integer year) {
            return valuesPerYear.getOrDefault(year, null);
        }

        /**
         * Updates data about total values with data for particular year.
         *
         * @param year       year
         * @param totalValue total value for given year
         * @return this
         */
        public TotalValuesPerYear update(final Integer year, final Double totalValue) {
            valuesPerYear.put(year, totalValue);
            return this;
        }
    }

    /**
     * Total values for particular buyers.
     */
    public static final class TotalValuesForBuyers {
        /**
         * Total values for particular buyers.
         */
        private final Map<String, TotalValuesPerYear> valuesForBuyers;

        /**
         * Constructor.
         */
        public TotalValuesForBuyers() {
            valuesForBuyers = new HashMap<>();
        }

        /**
         * Getter for total value for given buyer and year.
         *
         * @param buyerGroupId buyer group id
         * @param year         year
         * @return total value for given buyer and year or null if there are no data for given buyer and year
         */
        public Double getValueForBuyerPerYear(final String buyerGroupId, final Integer year) {
            // get values for given buyer
            TotalValuesPerYear valuesForBuyer = valuesForBuyers.getOrDefault(buyerGroupId, null);
            if (valuesForBuyer == null) {
                return null;
            }
            // get value for particular year
            return valuesForBuyer.getTotalValueForYear(year);
        }

        /**
         * Updates data about total values with data for particular buyer and year.
         *
         * @param buyerGroupId buyer group id
         * @param year         year
         * @param totalValue   total value for given buyer and year
         * @return this
         */
        public TotalValuesForBuyers update(final String buyerGroupId, final Integer year, final Double totalValue) {
            // get values for given buyer. If no data are available, create new record in map.
            TotalValuesPerYear valuesForBuyer = valuesForBuyers.getOrDefault(buyerGroupId, null);
            if (valuesForBuyer == null) {
                valuesForBuyer = new TotalValuesPerYear();
                valuesForBuyers.put(buyerGroupId, valuesForBuyer);
            }
            // update value for buyer
            valuesForBuyer.update(year, totalValue);
            return this;
        }

        /**
         * Checks if cache is empty.
         *
         * @return true if empty, false otherwise
         */
        public boolean isEmpty() {
            return valuesForBuyers.isEmpty();
        }
    }

    /**
     * Total values for particular suppliers.
     */
    public static final class TotalValuesForSuppliersByBuyers {
        /**
         * Total values for particular suppliers.
         */
        private final Map<String, TotalValuesForBuyers> valuesForSuppliers;

        /**
         * Constructor.
         */
        public TotalValuesForSuppliersByBuyers() {
            valuesForSuppliers = new HashMap<>();
        }

        /**
         * Getter for total value for given supplier, buyer and year.
         *
         * @param supplierGroupId supplier group id
         * @param buyerGroupId    buyer group id
         * @param year            year
         * @return total value for given supplier, buyer and year or null if there are no data for given supplier,
         * buyer and year
         */
        public Double getValueForSupplierPerBuyerAndYear(final String supplierGroupId, final String buyerGroupId,
                                                         final Integer year) {
            // get values for supplier
            TotalValuesForBuyers valuesForSupplier = valuesForSuppliers.getOrDefault(supplierGroupId, null);
            if (valuesForSupplier == null) {
                return null;
            }
            // get values for buyer and year
            return valuesForSupplier.getValueForBuyerPerYear(buyerGroupId, year);
        }

        /**
         * Updates data about total values with data for particular supplier, buyer and year.
         *
         * @param supplierGroupId supplier group id
         * @param buyerGroupId    buyer group id
         * @param year            year
         * @param totalValue      total value for given supplier, buyer and year
         * @return this
         */
        public TotalValuesForSuppliersByBuyers update(final String supplierGroupId, final String buyerGroupId,
                                                      final Integer year, final Double totalValue) {
            TotalValuesForBuyers valuesForSupplier = valuesForSuppliers.getOrDefault(supplierGroupId, null);
            if (valuesForSupplier == null) {
                valuesForSupplier = new TotalValuesForBuyers();
                valuesForSuppliers.put(supplierGroupId, valuesForSupplier);
            }
            valuesForSupplier.update(buyerGroupId, year, totalValue);
            return this;
        }

        /**
         * Checks if cache is empty.
         *
         * @return true if empty, false otherwise
         */
        public boolean isEmpty() {
            return valuesForSuppliers.isEmpty();
        }
    }

    /**
     * Caching values for buyers by years.
     */
    private final TotalValuesForBuyers totalValuesForBuyersPerYears;

    /**
     * Caching values for suppliers by buyers and years.
     */
    private final TotalValuesForSuppliersByBuyers totalValuesForSuppliersByBuyers;

    /**
     * Getter for totalValuesForBuyersPerYears.
     *
     * @return totalValuesForBuyersPerYears
     */
    public final TotalValuesForBuyers getCacheTotalValuesForBuyersPerYears() {
        return totalValuesForBuyersPerYears;
    }

    /**
     * Getter for totalValuesForSuppliersByBuyers.
     *
     * @return totalValuesForSuppliersByBuyers
     */
    public final TotalValuesForSuppliersByBuyers getCacheTotalValuesForSuppliersByBuyers() {
        return totalValuesForSuppliersByBuyers;
    }

    /**
     * Constructor.
     */
    public WinnerCAShareIndicatorPlugin() {
        totalValuesForBuyersPerYears = new TotalValuesForBuyers();
        totalValuesForSuppliersByBuyers = new TotalValuesForSuppliersByBuyers();
    }


    @Override
    public final Indicator evaluate(final MasterTenderLot masterLot, final MasterTender tender) {
        if (tender == null || masterLot == null || masterLot.getBids() == null || masterLot.getBids().isEmpty()
                || tender.getBuyers() == null || tender.getBuyers().size() != 1 || tender.getPublications() == null) {
            return insufficient();
        }

        // need winning bid to get supplier
        MasterBid winningBid =
                masterLot.getBids().stream().filter(Objects::nonNull)
                        .filter(a -> a.getIsWinning() != null && a.getIsWinning()).findFirst().orElse(null);
        if (winningBid == null || winningBid.getBidders() == null || winningBid.getBidders().size() != 1) {
            return insufficient();
        }

        String supplierGroupId = winningBid.getBidders().get(0).getGroupId();
        String buyerGroupId = tender.getBuyers().get(0).getGroupId();
        if (supplierGroupId == null || supplierGroupId.isEmpty() || buyerGroupId == null || buyerGroupId.isEmpty()) {
            return insufficient();
        }

        // need oldest publication date to get year
        LocalDate oldestPublicationDate =
                tender.getPublications().stream().filter(p -> p.getIsIncluded() != null && p.getIsIncluded())
                        .map(Publication::getPublicationDate).filter(Objects::nonNull).min(LocalDate::compareTo).orElse(null);
        if (oldestPublicationDate == null) {
            return insufficient();
        }
        Integer year = oldestPublicationDate.getYear();

        // values for computing indicator value, searching in cached data
        Double totalValueForSupplierByBuyerAndYear =
                totalValuesForSuppliersByBuyers.getValueForSupplierPerBuyerAndYear(supplierGroupId, buyerGroupId, year);
        Double totalValueForBuyerByYear =
                totalValuesForBuyersPerYears.getValueForBuyerPerYear(buyerGroupId, year);

        if (totalValueForSupplierByBuyerAndYear == null) {
            // getting list of master tenders which have particular supplier, buyer and year
            List<MasterTender> tenders = getTendersBySupplierAndBuyerAndYear(supplierGroupId, buyerGroupId, year);
            // computing total value
            totalValueForSupplierByBuyerAndYear = computeTotalValueForSupplierFromMasterTenders(tenders, supplierGroupId);
            // updating cached data
            totalValuesForSuppliersByBuyers.update(supplierGroupId, buyerGroupId, year, totalValueForSupplierByBuyerAndYear);
        }

        if (totalValueForBuyerByYear == null) {
            // getting list of master tenders which have particular buyer and year
            List<MasterTender> tenders = getTendersByBuyerAndYear(buyerGroupId, year);
            // computing total value
            totalValueForBuyerByYear = computeTotalValueForBuyerFromMasterTenders(tenders);
            // updating cached data
            totalValuesForBuyersPerYears.update(buyerGroupId, year, totalValueForBuyerByYear);
        }

        // calculation of indicator value based on total values for supplier and buyer
        if (totalValueForBuyerByYear == 0) {
            return insufficient();
        }
        return calculated(computeIndicatorValue(totalValueForSupplierByBuyerAndYear, totalValueForBuyerByYear));
    }

    /**
     * Gets list of master tenders with particular buyer and year.
     *
     * @param buyerGroupId buyer group id
     * @param year         year
     * @return list of master tenders
     */
    public final List<MasterTender> getTendersByBuyerAndYear(final String buyerGroupId, final Integer year) {
        JdbcMasterTenderIndicatorDAO masterDAO = new JdbcMasterTenderIndicatorDAO();
        return masterDAO.getBuyerTendersInPeriod(buyerGroupId,
                LocalDate.of(year, 1, 1), LocalDate.of(year, 12, 31));
    }

    /**
     * Gets list of master tenders with particular buyer and year.
     *
     * @param supplierGroupId supplier group id
     * @param buyerGroupId buyer group id
     * @param year         year
     * @return list of master tenders
     */
    public final List<MasterTender> getTendersBySupplierAndBuyerAndYear(final String supplierGroupId,
                                                                  final String buyerGroupId, final Integer year) {
        JdbcMasterTenderIndicatorDAO masterDAO = new JdbcMasterTenderIndicatorDAO();
        return masterDAO.getSupplierTendersInPeriodByBuyer(buyerGroupId, supplierGroupId,
                LocalDate.of(year, 1, 1), LocalDate.of(year, 12, 31));
    }

    /**
     * Computing total value as sum of prices of winning bids.
     *
     * @param tenders tenders
     * @return total value
     */
    public final Double computeTotalValueForBuyerFromMasterTenders(final List<MasterTender> tenders) {
        BigDecimal totalValue = tenders.stream().filter(Objects::nonNull).filter(t -> t.getLots() != null)
                .map(t -> t.getLots().stream().filter(Objects::nonNull).filter(l -> l.getBids() != null)
                        .map(l -> l.getBids().stream().filter(b -> b.getIsWinning() != null && b.getIsWinning())
                                .filter(b -> b.getPrice() != null && b.getPrice().getNetAmountNational() != null)
                                .map(b -> b.getPrice().getNetAmountNational()).reduce(BigDecimal.ZERO, BigDecimal::add))
                        .reduce(BigDecimal.ZERO, BigDecimal::add)).reduce(BigDecimal.ZERO, BigDecimal::add);
        return totalValue.doubleValue();
    }

    /**
     * Computing total value as sum of prices of winning bids with given supplier.
     *
     * @param tenders         tenders
     * @param supplierGroupId supplier group id
     * @return total value
     */
    public final Double computeTotalValueForSupplierFromMasterTenders(final List<MasterTender> tenders,
                                                                 final String supplierGroupId) {
        BigDecimal totalValue = tenders.stream().filter(Objects::nonNull).filter(t -> t.getLots() != null)
                .map(t -> t.getLots().stream().filter(Objects::nonNull).filter(l -> l.getBids() != null)
                        .map(l -> l.getBids().stream().filter(b -> b.getIsWinning() != null && b.getIsWinning())
                                .filter(b -> hasGivenSupplierAndPrice(b, supplierGroupId))
                                .map(b -> b.getPrice().getNetAmountNational()).reduce(BigDecimal.ZERO, BigDecimal::add))
                        .reduce(BigDecimal.ZERO, BigDecimal::add)).reduce(BigDecimal.ZERO, BigDecimal::add);
        return totalValue.doubleValue();
    }


    /**
     * Checks if given bid has particular supplier and price is not missing.
     *
     * @param bid             bid to check
     * @param supplierGroupId supplier group id
     * @return true if bid has particular supplier and price is not missing, false otherwise
     */
    public final boolean hasGivenSupplierAndPrice(final MasterBid bid, final String supplierGroupId) {
        if (bid == null || bid.getBidders() == null || bid.getBidders().size() != 1 || bid.getBidders().get(0) == null
                || bid.getPrice() == null || bid.getPrice().getNetAmountNational() == null) {
            return false;
        }
        String bidderGroupId = bid.getBidders().get(0).getGroupId();
        return bidderGroupId != null && bidderGroupId.equals(supplierGroupId);
    }

    /**
     * Computes indicator value.
     *
     * @param totalValueForSupplierByBuyerAndYear total value for supplier
     * @param totalValueForBuyerByYear            total value for buyer
     * @return indicator value
     */
    public final double computeIndicatorValue(final Double totalValueForSupplierByBuyerAndYear,
                                        final Double totalValueForBuyerByYear) {
        return 100 - ((totalValueForSupplierByBuyerAndYear / totalValueForBuyerByYear) * 100);
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.INTEGRITY_WINNER_CA_SHARE.name();
    }
}
