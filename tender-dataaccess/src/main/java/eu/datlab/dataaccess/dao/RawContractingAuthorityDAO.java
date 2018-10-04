package eu.datlab.dataaccess.dao;

import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Raw contracting authority DAO. Specifies methods for storing and loading raw
 * data (HTML, XML, ...) about contracting authorities.
 * 
 * @param <T>
 *            raw data
 */
public interface RawContractingAuthorityDAO<T extends RawData> extends RawDataDAO<T> {

}
