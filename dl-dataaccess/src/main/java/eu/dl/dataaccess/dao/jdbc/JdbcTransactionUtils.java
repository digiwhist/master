package eu.dl.dataaccess.dao.jdbc;

import java.sql.Connection;
import java.sql.SQLException;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.core.UnrecoverableException;
import eu.dl.core.config.Config;
import eu.dl.dataaccess.dao.TransactionUtils;

/**
 * Transaction handling for hibernate.
 */
public final class JdbcTransactionUtils implements TransactionUtils {

    private Connection connection;

    private static JdbcTransactionUtils instance;

    private Logger logger;

    private Config config;

    /**
     * Inits utils, creates connections etc.
     */
    private JdbcTransactionUtils() {
        try {
            config = Config.getInstance();

            logger = LoggerFactory.getLogger(this.getClass().getName());

            String url = config.getParam("jdbc.url");

            ComboPooledDataSource cpds = new ComboPooledDataSource();
            cpds.setJdbcUrl(url);
            cpds.setUser(config.getParam("jdbc.user"));
            cpds.setPassword(config.getParam("jdbc.password"));
            connection = cpds.getConnection();

            logger.info("Successfully established database connection to {}", url);
        } catch (Exception e) {
            logger.error("Unable to establish db connection caused by {}", e);
            throw new UnrecoverableException("Unable to establish db connection because of", e);
        }
    }

    /**
     * Returns initialised utils.
     * 
     * @return utils
     */
    public static JdbcTransactionUtils getInstance() {
        if (instance == null) {
            instance = new JdbcTransactionUtils();
        }

        return instance;
    }

    /**
     * Returns initialised entity manager.
     * 
     * @return entity manager
     */
    public Connection getConnection() {
        return connection;
    }

    @Override
    public void begin() {
        logger.debug("Transaction({}) is started by default.", connection.hashCode());
    }

    @Override
    public void commit() {
        try {
            if (!connection.getAutoCommit()) {
                connection.commit();
                logger.debug("Transaction({}) commited.", connection.hashCode());
            } else {
                logger.trace("Transaction is in autocommit mode, no commmit.");
            }
        } catch (SQLException ex) {
            logger.error("Unable to commit transaction.");
            throw new UnrecoverableException("Unable to commit transaction", ex);
        }
    }

    @Override
    public void rollback() {
        try {
            if (!connection.getAutoCommit()) {
                connection.rollback();
                logger.debug("Transaction({}) rollbacked.", connection.hashCode());
            } else {
                logger.trace("Transaction is in autocommit mode, no rollback needed.");
            }
        } catch (SQLException ex) {
            logger.error("Unable to rollback transaction.");
            throw new UnrecoverableException("Unable to rollback transaction", ex);
        }
    }

}
