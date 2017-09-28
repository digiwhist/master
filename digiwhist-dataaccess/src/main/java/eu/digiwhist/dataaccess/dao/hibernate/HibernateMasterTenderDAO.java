package eu.digiwhist.dataaccess.dao.hibernate;

import eu.dl.dataaccess.dao.MasterTenderDAO;
import eu.dl.dataaccess.dao.hibernate.GenericHibernateDAO;
import eu.dl.dataaccess.dto.master.MasterTender;

import javax.persistence.Query;
import java.util.Collection;
import java.util.List;

/**
 * Hibernate DAO implementation for tenders.
 */
public class HibernateMasterTenderDAO extends GenericHibernateDAO<MasterTender> implements
        MasterTenderDAO<MasterTender> {

    @Override
    protected final Class<MasterTender> getDTOClass() {
        return MasterTender.class;
    }

    @Override
    public final MasterTender getEmptyInstance() {
        return new MasterTender();
    }

    @Override
    public final List<MasterTender> getByGroupId(final String groupId) {
        Query q = entityManager.createQuery("SELECT e FROM " + getDTOClass().getName() + " e WHERE groupId = :groupId");
        q.setParameter("groupId", groupId);

        List<MasterTender> result = (List<MasterTender>) q.getResultList();

        return deserialize(result);
    }

    @Override
    public final List<MasterTender> getByGroupIds(final Collection<String> groupIds) {
        return null;
    }

    @Override
    public final List<MasterTender> getByCountry(final String countryCode, final Integer page) {
        return null;
    }
}
