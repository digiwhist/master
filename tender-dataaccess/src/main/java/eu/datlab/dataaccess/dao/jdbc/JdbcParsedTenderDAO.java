package eu.datlab.dataaccess.dao.jdbc;

import eu.dl.dataaccess.dao.ParsedTenderDAO;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;
import eu.dl.dataaccess.dto.parsed.ParsedTender;

/**
 * Hibernate DAO implementation for tenders.
 */
public class JdbcParsedTenderDAO extends GenericJdbcDAO<ParsedTender>
        implements ParsedTenderDAO<ParsedTender> {
    private static final String TABLE_NAME = "parsed_tender";

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public final ParsedTender getEmptyInstance() {
        return new ParsedTender();
    }
}
