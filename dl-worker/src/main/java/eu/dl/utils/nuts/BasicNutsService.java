package eu.dl.utils.nuts;

import eu.dl.dataaccess.dao.PostcodeNutsDAO;
import eu.dl.dataaccess.dao.jdbc.JdbcPostcodeNutsDAO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Tomas Mrazek
 */
public class BasicNutsService implements NutsService {
    private final Logger logger;

    private final PostcodeNutsDAO dao;

    /**
     * Default constructor.
     */
    public BasicNutsService() {
        dao = new JdbcPostcodeNutsDAO();
        logger = LoggerFactory.getLogger(this.getClass());
    }

    @Override
    public final String convert(final String country, final String postcode) {
        if (country == null || postcode == null) {
            logger.debug("Unable to convert postcode {} and country {} to NUTS, unsufficient data provided", postcode, country);
            return null;
        }

        return dao.getNutsByPostcode(postcode, country);
    }
}
