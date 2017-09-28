package eu.digiwhist.dataaccess.dao.jdbc;

import eu.digiwhist.dataaccess.dao.ParsedContractingAuthorityDAO;
import eu.digiwhist.dataaccess.dto.parsed.ParsedContractingAuthority;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;

/**
 * JDBC DAO implementation for contracting authority.
 */
public class JdbcParsedContractingAuthorityDAO extends GenericJdbcDAO<ParsedContractingAuthority>
        implements ParsedContractingAuthorityDAO<ParsedContractingAuthority> {
    private static final String TABLE_NAME = "parsed_contracting_authority";

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public final ParsedContractingAuthority getEmptyInstance() {
        return new ParsedContractingAuthority();
    }
}
