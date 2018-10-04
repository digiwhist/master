package eu.datlab.dataaccess.dao;

import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Raw asset declaration DAO. Specifies methods for storing and loading raw data about asset declarations.
 * 
 * @param <T>
 *            raw data
 */
public interface RawAssetDeclarationDAO<T extends RawData> extends RawDataDAO<T> {
}
