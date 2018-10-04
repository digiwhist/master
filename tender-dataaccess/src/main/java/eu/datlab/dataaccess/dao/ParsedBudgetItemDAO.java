package eu.datlab.dataaccess.dao;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.dl.dataaccess.dao.ParsedDAO;

/**
 * Parsed budget item DAO. Specifies methods for manipulating data about budget items that has been parsed.
 * 
 * @param <T>
 *         implementation class type that should be used for parsed budget item
 */
public interface ParsedBudgetItemDAO<T extends ParsedBudgetItem> extends ParsedDAO<T> {

}
