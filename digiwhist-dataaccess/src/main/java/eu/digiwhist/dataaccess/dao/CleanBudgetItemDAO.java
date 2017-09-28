package eu.digiwhist.dataaccess.dao;

import eu.digiwhist.dataaccess.dto.clean.CleanBudgetItem;
import eu.dl.dataaccess.dao.CleanDAO;

/**
 * Clean budget item DAO. Specifies methods for manipulating data about budget items that has been cleaned.
 * 
 * @param <T>
 *          implementation class type that should be used for clean budget item
 */
public interface CleanBudgetItemDAO<T extends CleanBudgetItem> extends CleanDAO<CleanBudgetItem> {

}
