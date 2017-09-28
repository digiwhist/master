package eu.digiwhist.dataaccess.dao.hibernate;

import java.util.Collection;
import java.util.List;

import javax.persistence.Query;

import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dao.hibernate.GenericHibernateDAO;
import eu.dl.dataaccess.dto.master.MasterBody;

/**
 * Hibernate DAO implementation for Master bodies.
 */
public class HibernateMasterBodyDAO extends GenericHibernateDAO<MasterBody> implements MasterBodyDAO<MasterBody> {

    @Override
    protected final Class<MasterBody> getDTOClass() {
        return MasterBody.class;
    }

    @Override
    public final MasterBody getEmptyInstance() {
        return new MasterBody();
    }

    @Override
    public final List<MasterBody> getByGroupId(final String groupId) {
        Query q = entityManager.createQuery("SELECT e FROM " + getDTOClass().getName()
                + " e WHERE groupId = :groupId");
        q.setParameter("groupId", groupId);

        List<MasterBody> result = (List<MasterBody>) q.getResultList();

        return deserialize(result);
    }

    @Override
    public final List<MasterBody> getByGroupIds(final Collection<String> groupIds) {
        return null;
    }
}
