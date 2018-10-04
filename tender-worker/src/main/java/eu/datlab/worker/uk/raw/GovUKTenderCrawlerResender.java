package eu.datlab.worker.uk.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.raw.BaseRawWorker;

import java.util.List;

/**
 * GovUK Tender Resender. This worker is for resend purposes only as GovUkTenderCrawler is saving raw data directly and
 * can not resend messages himself.
 */
public final class GovUKTenderCrawlerResender extends BaseRawWorker {
    private static final String INCOMING_EXCHANGE_NAME = "raw";

    @Override
    protected String getVersion() {
        return null;
    }

    @Override
    protected String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

    @Override
    protected String getIncomingExchangeName() {
        return INCOMING_EXCHANGE_NAME;
    }

    @Override
    protected void doWork(final Message message) {
        throw new UnrecoverableException("This worker is only for resend.");
    }

    @Override
    protected void resend(final String version, final String dateFrom, final String dateTo) {
        logger.debug("Resending messages to be parsed.");

        try {
            final List<Raw> rawDataItems = getRawDataDao().getMine(GovUKTenderCrawler.class.getName(),
                    GovUKTenderCrawler.getStaticVersion(), dateFrom, dateTo);

            for (final Raw rawDataItem : rawDataItems) {
                final Message outgoingMessage = MessageFactory.getMessage();
                outgoingMessage.setValue("id", rawDataItem.getId());
                publishMessage(outgoingMessage, GovUKTenderCrawler.class.getName());
            }
        } catch (final Exception ex) {
            logger.error("Unable to resend messages for parsing {}", ex);
            throw new UnrecoverableException("Unable to resend messages for parsing", ex);
        }
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    /**
     * Get raw dao to access raw_data.
     *
     * @return RawDAO
     */
    private RawDAO<Raw> getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(GovUKTenderCrawler.class.getName(),
                GovUKTenderCrawler.getStaticVersion());
    }
}
