package eu.digiwhist.dataaccess.dao.hibernate;

import java.util.List;

import javax.persistence.Query;

import eu.dl.dataaccess.dao.ManualMatchDAO;
import eu.dl.dataaccess.dao.hibernate.GenericHibernateDAO;
import eu.dl.dataaccess.dto.matched.ManualMatch;

/**
 * DAO used to check for manual matches.
 * 
 * @param <T>
 *            manual match type
 */
public class HibernateManualMatchDAO<T extends ManualMatch> extends GenericHibernateDAO<ManualMatch>
        implements ManualMatchDAO<ManualMatch> {

    @Override
    public final List<ManualMatch> getByHash(final String hash, final String flag) {
        Query q = entityManager.createQuery("SELECT e FROM " + getDTOClass().getName() + " e "
                + "WHERE hash = :hash AND flag = :flag");
        q.setParameter("hash", hash);
        q.setParameter("flag", flag);

        return (List<ManualMatch>) q.getResultList();
    }

    @Override
    public final ManualMatch getEmptyInstance() {
        return new ManualMatch();
    }

    @Override
    protected final Class<ManualMatch> getDTOClass() {
        // TODO Auto-generated method stub
        return ManualMatch.class;
    }

	@Override
	public final List<ManualMatch> getAllEntries(final String flag) {
		// TODO Auto-generated method stub
		return null;
	}

}
