package eu.digiwhist.dataaccess.dao.hibernate;

import eu.dl.dataaccess.dao.ParsedTenderDAO;
import eu.dl.dataaccess.dao.hibernate.GenericHibernateDAO;
import eu.dl.dataaccess.dto.parsed.ParsedTender;

/**
 * Hibernate DAO implementation for tenders.
 */
public class HibernateParsedTenderDAO extends GenericHibernateDAO<ParsedTender>
        implements ParsedTenderDAO<ParsedTender> {

    @Override
    protected final Class<ParsedTender> getDTOClass() {
        return ParsedTender.class;
    }

    @Override
    public final ParsedTender getEmptyInstance() {
        return new ParsedTender();
    }
}
