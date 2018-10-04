package eu.datlab.dataaccess.dao.jdbc;

import eu.datlab.dataaccess.dao.ParsedPublicOfficialDAO;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;
import eu.dl.dataaccess.dto.parsed.ParsedPublicOfficial;

/**
 * JDBC DAO implementation for contracting authority.
 */
public class JdbcParsedPublicOfficialDAO extends GenericJdbcDAO<ParsedPublicOfficial>
        implements ParsedPublicOfficialDAO<ParsedPublicOfficial> {
    private static final String TABLE_NAME = "parsed_public_official";

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public final ParsedPublicOfficial getEmptyInstance() {
        return new ParsedPublicOfficial();
    }
}
