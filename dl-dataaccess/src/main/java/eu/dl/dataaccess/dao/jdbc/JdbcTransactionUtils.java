package eu.dl.dataaccess.dao.jdbc;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

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

            Class.forName("org.postgresql.Driver");

            String url = config.getParam("jdbc.url");

            connection = DriverManager.getConnection(url, config.getParam("jdbc.user"),
                    config.getParam("jdbc.password"));

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
