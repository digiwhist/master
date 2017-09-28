package eu.digiwhist.worker.parser;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.dl.dataaccess.dao.ParsedDAO;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.parsed.BaseParser;

/**
 * Base class for Digiwhist budget item parsers.
 */
public abstract class BaseDigiwhistBudgetItemParser extends BaseParser<RawData, ParsedBudgetItem> {

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
