package eu.datlab.worker.parser;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.dl.dataaccess.dao.ParsedDAO;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.parsed.BaseParser;

/**
 * Base class for budget item parsers.
 */
public abstract class BaseDatlabBudgetItemParser extends BaseParser<RawData, ParsedBudgetItem> {

    @Override
    protected final RawDAO<RawData> getRawDAO() {
        return DAOFactory.getDAOFactory().getRawBudgetItemDAO(getName(), getVersion());
    }

    @Override
    protected final ParsedDAO<ParsedBudgetItem> getParsedDAO() {
        return DAOFactory.getDAOFactory().getParsedBudgetItemDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
