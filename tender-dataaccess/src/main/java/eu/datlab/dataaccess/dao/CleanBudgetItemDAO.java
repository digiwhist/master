package eu.datlab.dataaccess.dao;

import eu.datlab.dataaccess.dto.clean.CleanBudgetItem;
import eu.dl.dataaccess.dao.CleanDAO;

import java.util.List;

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
     * @return list of body budget items from the given period of years
     */
    List<T> getBodyBudgetItemsInPeriod(String organizationId, int yearFrom, int yearTo);
}
