package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Raw tender DAO interface. Specifies methods for storing and loading raw data
 * about tenders (source data like HTML or XML code).
 * 
 * @param <T>
 *            implementation class type that should be used for parsed tender
 */
public interface RawDataDAO<T extends RawData> extends RawDAO<T> {
}
