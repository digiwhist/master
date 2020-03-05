package eu.datlab.worker.cz.utils;

import eu.dl.core.config.Config;
import java.util.HashSet;
import java.util.Set;

/**
 * ICOs blacklist.
 *
 * @author Tomas Mrazek
 */
public enum StatniPokladnaBlacklist {
    /**
     * Instance.
     */
    INSTANCE;
    
    /**
     * The name of configuration property that holds ICOs balcklist (comma separated).
     */
    private static final String CONFIGURATION_PROPERTY_NAME = "StatniPokladnaBudget.blacklist";

    /**
     * Blacklist.
     */
    private Set<String> blacklist = null;
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    StatniPokladnaBlacklist() {};

    /**
     * Provides instance of Config class(singleton pattern).
     *
     * @return initiated instance of Config
     */
    public static StatniPokladnaBlacklist getInstance() {
        return INSTANCE;
    }

    /**
     * Checks whether the given ico is on the blacklist.
     *
     * @param ico
     *      tested ico
     * @return true only and only if the ico is on blacklist, otherwise false
     */
    public boolean isBlack(final String ico) {
        if (blacklist == null) {
            blacklist = Config.getInstance().getParamValueAsList(CONFIGURATION_PROPERTY_NAME, ",", HashSet.class);
        }

        return blacklist.contains(ico);
    }
}
