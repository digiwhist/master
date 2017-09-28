package eu.dl.dataaccess.dao.mongo;

import eu.dl.dataaccess.dao.TransactionUtils;


/**
 * Transaction handling for mongo. Currently dummy implementation not handling
 * anythong.
 */
public final class MongoTransactionUtils implements TransactionUtils {

    private static MongoTransactionUtils instance;

    /**
     * Inits utils.
     */
    private MongoTransactionUtils() {
    }

    /**
     * Returns initialised utils.
     * 
     * @return utils
     */
    public static MongoTransactionUtils getInstance() {
        if (instance == null) {
            instance = new MongoTransactionUtils();
        }

        return instance;
    }

    @Override
    public void begin() {

    }

    @Override
    public void commit() {

    }

    @Override
    public void rollback() {

    }

}
