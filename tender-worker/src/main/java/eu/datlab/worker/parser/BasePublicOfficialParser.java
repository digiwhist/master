package eu.datlab.worker.parser;

import java.util.List;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.ParsedDAO;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.parsed.ParsedPublicOfficial;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.parsed.BaseParser;

/**
 * Base public official parsed.
 *
 * @author Michal Riha
 */
public abstract class BasePublicOfficialParser extends BaseParser<RawData, ParsedPublicOfficial> {

    @Override
    protected final RawDAO<RawData> getRawDAO() {
        return DAOFactory.getDAOFactory().getRawPublicOfficialDAO(getName(), getVersion());
    }

    @Override
    protected final ParsedDAO<ParsedPublicOfficial> getParsedDAO() {
        return DAOFactory.getDAOFactory().getParsedPublicOfficialDAO(getName(), getVersion());
    }

    @Override
    protected final List<ParsedPublicOfficial> postProcess(final List<ParsedPublicOfficial> parsedItems,
        final RawData raw) {
        return parsedItems;
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
