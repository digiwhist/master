package eu.datlab.dataaccess.dao;

import eu.datlab.dataaccess.dto.zindex.ZIndexIndicator;
import eu.datlab.dataaccess.dto.zindex.ZIndexOffense;
import eu.dl.dataaccess.dao.GenericDAO;

import java.util.List;

/**
 * ZIndex indicator DAO.
 *
 * @author Tomas Mrazek
 */
public interface ZIndexIndicatorDAO extends GenericDAO<ZIndexIndicator> {
    /**
     * Returns list of zindex indicators with the given name.
     *
     * @param name
     *      indicator name
     * @return list of indicators
     */
    List<ZIndexIndicator> getByName(String name);

    /**
     * Returns all calculated zindex indicators.
     *
     * @return list of indicators
     */
    List<ZIndexIndicator> getAll();

    /**
     * Returns list of offenses for the given organization id and year.
     *
     * @param organizationId
     *      organization id of the subject for witch the offenses are looked for
     * @param yearFrom
     *      start year of period when the offense have occurred
     * @param yearTo
     *      end year of period when the offense have occurred
     * @return list of offenses
     */
    List<ZIndexOffense> getOffenses(String organizationId, Integer yearFrom, Integer yearTo);
}
