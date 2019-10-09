package eu.datlab.dataaccess.dao;

import eu.datlab.dataaccess.dto.clean.CleanBudgetItem;
import eu.datlab.dataaccess.dto.codetables.BudgetItemReportType;
import eu.dl.dataaccess.dao.CleanDAO;

import java.util.List;
import java.util.Map;

/**
 * Clean budget item DAO. Specifies methods for manipulating data about budget items that has been cleaned.
 * 
 * @param <T>
 *          implementation class type that should be used for clean budget item
 */
public interface CleanBudgetItemDAO<T extends CleanBudgetItem> extends CleanDAO<CleanBudgetItem> {
    /**
     * Returns body budget items from the given period of years. Budget items must be one of the following types A.I.1 - A.I.12, A.I.35,
     * A.I.36, or B.I..
     *
     * @param organizationId
     *      organization id of the body whose budget items are looked for
     * @param yearFrom
     *      start year of period
     * @param yearTo
     *      end year of period
     * @param reportsAndCodes
     *      report types with list of level 3 codes to be used for filtering. List can be null or empty, in such case only the report
     *      type name is used for filtering
     * @return list of body budget items from the given period of years
     */
    List<T> getBodyBudgetItemsInPeriod(String organizationId, int yearFrom, int yearTo, Map<BudgetItemReportType,
        List<String>> reportsAndCodes);
}
