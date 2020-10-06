package eu.dl.dataaccess.dao;

/**
 * Transaction utils used when there is no transaction handling needed at all.
 * 
 * @author skajrajdr
 *
 */
public class DummyTransactionUtils implements TransactionUtils {

    @Override
    public void begin() {
        // TODO Auto-generated method stub

    }

    @Override
    public void commit() {
        // TODO Auto-generated method stub

    }

    @Override
    public void rollback() {
        // TODO Auto-generated method stub

    }

}
