package eu.datlab.worker.system;

import java.util.List;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.MasterTenderDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.BaseWorker;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;

/**
 * This worker helpes to generate messages for statistic calulations workers.
 * 
 * @author Jakub Krafka
 */
public final class StatisticCalculationPlanner extends BaseWorker {

    private static final String INCOMING_EXCHANGE_NAME = "init";

    private static final String OUTGOING_EXCHANGE_NAME = "master";

    private static final String VERSION = "1.0";

    private static TransactionUtils transactionUtils;

    private static MasterTenderDAO masterDao;

    /**
     * Initialization of everythong.
     */
    public StatisticCalculationPlanner() {
        super();
        transactionUtils = DAOFactory.getDAOFactory().getTransactionUtils();

        masterDao = DAOFactory.getDAOFactory().getMasterTenderDAO(getName(), VERSION);
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected String getIncomingExchangeName() {
        return INCOMING_EXCHANGE_NAME;
    }
    
    @Override
    protected String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

    @Override
    protected String getOutgoingExchangeName() {
        return OUTGOING_EXCHANGE_NAME;
    }

    @Override
    public void doWork(final Message message) {
        String target = message.getValue("target");
        String source = message.getValue("source");
        String version = message.getValue("version");

        final List<String> result = masterDao.getIdsBySourceAndVersion(source, version);

        if (result != null) {
            for (String id : result) {
                Message outgoingMessage = MessageFactory.getMessage();
                outgoingMessage.setValue("id", id);
                publishMessage(outgoingMessage, target);
            }
        }
    }

    @Override
    protected void resend(final String version, final String dateFrom, final String dateTo) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
