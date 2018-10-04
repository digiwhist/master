package eu.datlab.dataaccess.dao;

import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Raw budget item DAO. Specifies methods for storing and loading raw data (HTML, XML, ...) about budgets.
 * 
 * @param <T>
 *            raw data
 */
public interface RawBudgetItemDAO<T extends RawData> extends RawDataDAO<T> {

}
