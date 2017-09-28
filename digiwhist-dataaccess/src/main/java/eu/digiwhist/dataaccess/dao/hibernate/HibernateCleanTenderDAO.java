package eu.digiwhist.dataaccess.dao.hibernate;

import eu.dl.dataaccess.dao.CleanTenderDAO;
import eu.dl.dataaccess.dao.hibernate.GenericHibernateDAO;
import eu.dl.dataaccess.dto.clean.CleanTender;

import java.time.LocalDate;
import java.util.List;

/**
 * Hibernate DAO implementation for tenders.
 */
public class HibernateCleanTenderDAO extends GenericHibernateDAO<CleanTender> implements CleanTenderDAO<CleanTender> {

    @Override
    protected final Class<CleanTender> getDTOClass() {
        return CleanTender.class;
    }

    @Override
    public final CleanTender getEmptyInstance() {
        return new CleanTender();
    }

    @Override
    public final List<CleanTender> getByCountry(final String countryCode, final Integer page) {
        return null;
    }

    @Override
    public final List<String> getIncludedPublicationSourceIds(final LocalDate date) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
