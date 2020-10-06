package eu.dl.worker.raw;

import eu.dl.core.UnrecoverableException;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.utils.NetworkUtils;

import java.util.HashMap;

/**
 * Base class for all the crawlers.
 */
public abstract class BaseCrawler extends BaseRawWorker {

    private static final String INCOMING_EXCHANGE_NAME = "init";

    /**
     * Default constructor.
     */
    public BaseCrawler() {
        // check whether TOR should be started
        if (config.getParam(getName() + ".torEnabled") != null
                && config.getParam(getName() + ".torEnabled").equals("1")) {
            NetworkUtils.enableTorForHttp();
        }
    }

    /**
     * Publishes new message with url to outgoing queue.
     *
     * @param url
     *         url to be downloaded
     */
    protected final void createAndPublishMessage(final String url) {
        final Message outgoingMessage = MessageFactory.getMessage();
        outgoingMessage.setValue("url", url);
        publishMessage(outgoingMessage);
        logger.info("New message sent to be processed: {}", outgoingMessage);
    }

    /**
     * Publishes new message with url and content (downloader only saves it to database).
     *
     * @param url
     *         source url
     * @param content
     *         downloaded content
     */
    protected final void createAndPublishMessage(final String url, final String content) {
        final Message outgoingMessage = MessageFactory.getMessage();
        outgoingMessage.setValue("url", url);
        outgoingMessage.setValue("sourceData", content);
        publishMessage(outgoingMessage);
        logger.info("New message sent to be processed: {}", outgoingMessage);
    }

    /**
     * Publishes new message with url and metadata to outgoing queue.
     *
     * @param url
     *         url to be downloaded
     * @param metaData
     *         message meta data
     */
    protected final void createAndPublishMessage(final String url, final HashMap<String, Object> metaData) {
        final Message outgoingMessage = MessageFactory.getMessage();
        outgoingMessage.setValue("url", url);
        outgoingMessage.setMetaData(metaData);
        publishMessage(outgoingMessage);
        logger.info("New message sent to be processed: {}", outgoingMessage);
    }

    /**
     * Publishes new message with url, content and metadata to outgoing queue.
     *
     * @param url
     *         url to be downloaded
     * @param content
     *         downloaded content
     * @param metaData
     *         message meta data
     */
    protected final void createAndPublishMessage(final String url, final String content,
            final HashMap<String, Object> metaData) {
        final Message outgoingMessage = MessageFactory.getMessage();
        outgoingMessage.setValue("url", url);
        outgoingMessage.setValue("sourceData", content);
        outgoingMessage.setMetaData(metaData);
        publishMessage(outgoingMessage);
        logger.info("New message sent to be processed: {}", outgoingMessage);
    }

    @Override
    protected final void resend(final String version, final String dateFrom, final String dateTo) {
        throw new UnrecoverableException("Crawler does not support message resending.");
    }

    @Override
    protected final String getIncomingExchangeName() {
        return INCOMING_EXCHANGE_NAME;
    }

    @Override
    protected final String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

}
