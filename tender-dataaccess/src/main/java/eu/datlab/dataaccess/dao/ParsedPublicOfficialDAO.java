package eu.datlab.dataaccess.dao;

import eu.dl.dataaccess.dao.ParsedDAO;
import eu.dl.dataaccess.dto.parsed.ParsedPublicOfficial;

/**
 * Parsed public official DAO interface. Specifies methods for manipulating data about
 * public officials that has been parsed (but not typed yet).
 * 
 * @param <T>
 *          implementation class type that should be used for parsed public official
 */
public interface ParsedPublicOfficialDAO<T extends ParsedPublicOfficial> extends ParsedDAO<T> {

}
