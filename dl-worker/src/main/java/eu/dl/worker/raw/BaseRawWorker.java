package eu.dl.worker.raw;

import eu.dl.worker.BaseWorker;

/**
 * Common paretn for all raw workers.
 */
public abstract class BaseRawWorker extends BaseWorker {

    private static final String OUTGOING_EXCHANGE_NAME = "raw";

    @Override
    protected final String getOutgoingExchangeName() {
        return OUTGOING_EXCHANGE_NAME;
    }
}
