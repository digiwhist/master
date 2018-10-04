package eu.datlab.dataaccess.dao;

import eu.datlab.dataaccess.dto.parsed.ParsedContractingAuthority;
import eu.dl.dataaccess.dao.ParsedDAO;

/**
 * Contracting Authorities DAO interface. Specifies methods for manipulating
 * parsed data about contract authorities.
 * 
 * @param <T>
 *          implementation class type that should be used for parsed contracting authority
 */
public interface ParsedContractingAuthorityDAO<T extends ParsedContractingAuthority> extends ParsedDAO<T> {

}
