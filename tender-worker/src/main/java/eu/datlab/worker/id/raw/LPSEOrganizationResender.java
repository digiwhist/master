package eu.datlab.worker.id.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.BaseWorker;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;

import java.util.List;

/**
 * Worker generates messages for {@link eu.datlab.worker.id.raw.LPSETenderCrawler} from
 * {@link eu.datlab.worker.id.raw.LPSEOrganizationDownloader} raw data.
 *
 * @author Tomas Mrazek
 */
public final class LPSEOrganizationResender extends BaseWorker {

    /**
     * Worker version.
     */
    public static final String VERSION = "1.0";

    private static final String OUTGOING_EXCHANGE_NAME = "init";

    private static final String INCOMING_EXCHANGE_NAME = "init";

    private static final String ORGANIZATION_WORKER_NAME = LPSEOrganizationDownloader.class.getName();

    private static final String ORGANIZATION_WORKER_VERSION = LPSEOrganizationDownloader.VERSION;

    private final RawDataDAO<RawData> organizationDAO;

    /**
     * Raw DAO initialization.
     */
    public LPSEOrganizationResender() {
        super();

        organizationDAO = DAOFactory.getDAOFactory().getRawTenderDAO(ORGANIZATION_WORKER_NAME, ORGANIZATION_WORKER_VERSION);
    }

    @Override
    protected void doWork(final Message message) {
        try {
            getTransactionUtils().begin();

            List<RawData> data = organizationDAO.getMine(ORGANIZATION_WORKER_NAME, ORGANIZATION_WORKER_VERSION, null, null);
            for (RawData r : data) {
                createAndPublishMessage(r.getId());
            }

            getTransactionUtils().commit();
        } catch (final Exception e) {
            logger.error("Crawling failed with exception", e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
    }

    /**
     * Publishes new message with id to outgoing queue.
     *
     * @param id
     *         id of resent raw record
     */
    private void createAndPublishMessage(final String id) {
        final Message outgoingMessage = MessageFactory.getMessage();
        outgoingMessage.setValue("id", id);
        publishMessage(outgoingMessage);
        logger.info("New message sent to be processed: {}", outgoingMessage);
    }

    @Override
    protected void resend(final String version, final String dateFrom, final String dateTo) {
        throw new UnrecoverableException("Resender does not support message resending.");
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
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
    protected String getIncomingExchangeName() {
        return INCOMING_EXCHANGE_NAME;
    }
}
