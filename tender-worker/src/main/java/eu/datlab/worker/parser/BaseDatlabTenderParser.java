package eu.datlab.worker.parser;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.ParsedDAO;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.parsed.BaseTenderParser;

/**
 * Base class for tender parsers.
 *
 */
public abstract class BaseDatlabTenderParser extends BaseTenderParser<RawData, ParsedTender> {

    @Override
    protected final RawDAO<RawData> getRawDAO() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    @Override
    protected final ParsedDAO<ParsedTender> getParsedDAO() {
        return DAOFactory.getDAOFactory().getParsedTenderDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
