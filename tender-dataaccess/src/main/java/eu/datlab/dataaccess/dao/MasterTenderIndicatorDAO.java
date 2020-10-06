package eu.datlab.dataaccess.dao;

import eu.dl.dataaccess.dao.MasterTenderDAO;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.time.LocalDate;
import java.util.List;

/**
 * Interface for collecting master tenders for computing indicators.
 */
public interface MasterTenderIndicatorDAO extends MasterTenderDAO<MasterTender> {
    /**
     * Returns all tenders for the given buyer group id which have the first included publication
     * published between {@code from} and {@code to} dates.
     *
     * @param buyerGroupId
     *      buyer group id
     * @param from
     *      from date
     * @param to
     *      to date
     * @return list of buyer's tenders
     */
    List<MasterTender> getBuyerTendersInPeriod(String buyerGroupId, LocalDate from, LocalDate to);

    /**
     * Returns all tenders for the given supplier group id and buyer group id which have the first included publication
     * published between {@code from} and {@code to} dates.
     *
     * @param buyerGroupId
     *      buyer group id
     * @param supplierGroupId
     *      supplier group id
     * @param from
     *      from date
     * @param to
     *      to date
     * @return list of buyer's tenders
     */
    List<MasterTender> getSupplierTendersInPeriodByBuyer(String buyerGroupId, String supplierGroupId, LocalDate from,
                                                         LocalDate to);

}
