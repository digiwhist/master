package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.parsed.ParsedTender;

import java.util.List;

/**
 * Parsed tender DAO interface. Specifies methods for manipulating data about
 * tenders that has been parsed (but not typed yet).
 *
 * @param <T>
 *         implementation class type that should be used for parsed tender
 */
public interface ParsedTenderDAO<T extends ParsedTender> extends ParsedDAO<T> {
    /**
     * Checks which hashes already exist in database from the given list.
     *
     * @param hashes
     *      list of hashes to be looked for
     * @return list of existing hashes or empty list
     */
    List<String> existsHash(List<String> hashes);
}
