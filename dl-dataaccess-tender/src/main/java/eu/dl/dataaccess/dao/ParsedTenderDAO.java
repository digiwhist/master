package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.parsed.ParsedTender;

/**
 * Parsed tender DAO interface. Specifies methods for manipulating data about
 * tenders that has been parsed (but not typed yet).
 *
 * @param <T>
 *         implementation class type that should be used for parsed tender
 */
public interface ParsedTenderDAO<T extends ParsedTender> extends ParsedDAO<T> {

}
