package eu.dl.worker.matched;

import eu.dl.worker.BaseWorker;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;

/**
 * Base class for all the matchers.
 */
public abstract class BaseMatcher extends BaseWorker {
    private static final String OUTGOING_EXCHANGE_NAME = "matched";

    private static final String INCOMING_EXCHANGE_NAME = "clean";

    /**
     * Publishes new message with id of saved matched entity (eg. tender, contracting authority) to outgoing queue.
     *
     * @param savedId
     *         id of the saved entity (eg. tender, contracting authority)
     */
    public final void createAndPublishMessage(final String savedId) {
        final Message outgoingMessage = MessageFactory.getMessage();
        outgoingMessage.setValue("id", savedId);
        logger.debug("Matching finished, publishing message {}", outgoingMessage);
        publishMessage(outgoingMessage);
    }

    @Override
    protected final String getOutgoingExchangeName() {
        return OUTGOING_EXCHANGE_NAME;
    }

    @Override
    protected final String getIncomingExchangeName() {
        return INCOMING_EXCHANGE_NAME;
    }
}
