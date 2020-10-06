package eu.dl.dataaccess.dao;

/**
 * Allows to work with transactions for underlying connection without dependency
 * on current DAO implementation.
 */
public interface TransactionUtils {
    /**
     * Closes connections.
     */
    void close();

    /**
     * Begins new transaction.
     */
    void begin();

    /**
     * Commits trnasaction.
     */
    void commit();

    /**
     * Rollbacks transaction.
     */
    void rollback();
}
