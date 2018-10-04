package eu.datlab.worker.parser;

import java.util.List;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.dataaccess.dto.parsed.ParsedContractingAuthority;
import eu.dl.dataaccess.dao.ParsedDAO;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.parsed.BaseParser;

/**
 * Base class for contracting authority parsers.
 *
 */
public abstract class BaseContractingAuthorityParser extends BaseParser<RawData, ParsedContractingAuthority> {

    @Override
    protected final RawDAO<RawData> getRawDAO() {
        return DAOFactory.getDAOFactory().getRawContractingAuthorityDAO(getName(), getVersion());
    }

    @Override
    protected final ParsedDAO<ParsedContractingAuthority> getParsedDAO() {
        return DAOFactory.getDAOFactory().getParsedContractingAuthorityDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @Override
    protected final List<ParsedContractingAuthority> postProcess(final List<ParsedContractingAuthority> parsedItems,
        final RawData raw) {
        return parsedItems;
    }
}
