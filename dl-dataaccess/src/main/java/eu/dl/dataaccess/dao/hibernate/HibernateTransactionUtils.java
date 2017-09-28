package eu.dl.dataaccess.dao.hibernate;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.FlushModeType;
import javax.persistence.Persistence;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dao.TransactionUtils;

/**
 * Transaction handling for hibernate.
 */
public final class HibernateTransactionUtils implements TransactionUtils {

    private EntityManager entityManager;

    private static HibernateTransactionUtils instance;

    private Logger logger;
    
    /**
     * Inits HibernateDButilsm creates connection etc.
     */
    private HibernateTransactionUtils() {
        Config config = Config.getInstance();
        logger = LoggerFactory.getLogger(this.getClass().getName());
        EntityManagerFactory entityManagerFactory = Persistence
                .createEntityManagerFactory(config.getParam("hibernate.entityManager"));
        entityManager = entityManagerFactory.createEntityManager();
        entityManager.setFlushMode(FlushModeType.COMMIT);
    }

    /**
     * Returns initialised utils.
     * 
     * @return utils
     */
    public static HibernateTransactionUtils getInstance() {
        if (instance == null) {
            instance = new HibernateTransactionUtils();
        }

        return instance;
    }

    /**
     * Returns initialised entity manager.
     * 
     * @return entity manager
     */
    public EntityManager getEntityManager() {
        return entityManager;
    }

    @Override
    public void begin() {
        if (!entityManager.getTransaction().isActive()) {
            entityManager.getTransaction().begin();
            logger.debug("Transaction({}) started.", entityManager.getTransaction().hashCode());
        } else {
            logger.debug("Transaction({}) already running.", entityManager.getTransaction().hashCode());
        }
    }

    @Override
    public void commit() {
        entityManager.getTransaction().commit();
        entityManager.clear();
        logger.debug("Transaction({}) commited.", entityManager.getTransaction().hashCode());
    }

    @Override
    public void rollback() {
        if (entityManager.getTransaction().isActive()) {
            entityManager.getTransaction().rollback();
            logger.debug("Transaction({}) rollback.", entityManager.getTransaction().hashCode());
        } else {
            logger.warn("Transaction rollback required but not performed as trasaction is not in active state.");
        }
    }

}
