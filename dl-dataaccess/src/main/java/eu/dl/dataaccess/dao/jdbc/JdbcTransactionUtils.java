package eu.dl.dataaccess.dao.jdbc;

import eu.dl.core.UnrecoverableException;
import eu.dl.core.config.Config;
import eu.dl.dataaccess.dao.TransactionUtils;
import org.apache.commons.dbcp2.BasicDataSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.Connection;
import java.sql.SQLException;

/**
 * Transaction handling for hibernate.
 */
public final class JdbcTransactionUtils implements TransactionUtils {

    private Connection connection;

    private BasicDataSource connectionPool;

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


            connectionPool = new BasicDataSource();

            connectionPool.setUsername(config.getParam("jdbc.user"));
            connectionPool.setPassword(config.getParam("jdbc.password"));
            connectionPool.setDriverClassName("org.postgresql.Driver");
            connectionPool.setUrl(config.getParam("jdbc.url"));
            connectionPool.setInitialSize(3);

            logger.info("Successfully established database connection to database");
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
        try {
            if (connection != null && !connection.isClosed()) {
                return connection;
            } else {
                connection = connectionPool.getConnection();
                return connection;
            }
        } catch (Exception e) {
            logger.error("Unable to return db connection caused by {}", e);
            throw new UnrecoverableException("Unable to return db connection because of", e);
        }
    }

    @Override
    public void close() {
        try {
            if (connection != null && !connection.isClosed()) {
                connection.close();
                logger.debug("DB connection closed");
            }
        } catch (SQLException ex) {
            logger.error("Unable to close connection.");
            throw new UnrecoverableException("Unable to close connection.", ex);
        }

    }

    @Override
    public void begin() {
        logger.debug("Transaction is started by default.");
    }

    @Override
    public void commit() {
        try {
            if (connection != null && !connection.isClosed() && !connection.getAutoCommit()) {
                connection.commit();
                logger.debug("Transaction({}) commited.", connection.hashCode());
            } else {
                logger.trace("Transaction is not in commmitable mode.");
            }
        } catch (SQLException ex) {
            logger.error("Unable to commit transaction.");
            throw new UnrecoverableException("Unable to commit transaction", ex);
        }
    }

    @Override
    public void rollback() {
        try {
            if (connection != null && !connection.isClosed() && !connection.getAutoCommit()) {
                connection.rollback();
                logger.debug("Transaction({}) rollbacked.", connection.hashCode());
            } else {
                logger.trace("Transaction is in not in rollbackable state.");
            }
        } catch (SQLException ex) {
            logger.error("Unable to rollback transaction.");
            throw new UnrecoverableException("Unable to rollback transaction", ex);
        }

    }

}
