package eu.datlab.dataaccess.dao;

import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Raw public official DAO. Specifies methods for storing and loading raw data
 * (HTML, XML, ...) about public officials.
 * 
 * @param <T>
 *            raw data
 */
public interface RawPublicOfficialDAO<T extends RawData> extends RawDataDAO<T> {

}
