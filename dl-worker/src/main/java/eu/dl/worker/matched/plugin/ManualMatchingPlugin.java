package eu.dl.worker.matched.plugin;

import java.util.List;

import org.cache2k.Cache;
import org.cache2k.Cache2kBuilder;

import eu.dl.dataaccess.dao.ManualMatchDAO;
import eu.dl.dataaccess.dto.matched.ManualMatch;
import eu.dl.dataaccess.dto.matched.ManuallyMatchable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This plugin checks, whether there are some manual fixes entered for the
 * particular CleanBody.
 *
 * @param <T>
 *
 */
public class ManualMatchingPlugin<T extends ManuallyMatchable> implements MatchingPlugin<T> {
    
    protected final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    private ManualMatchDAO manualMatchDao;

    private static final String MATCHED_BY = "manual";

    private String flag = "";

    protected final Cache<String, String> cache; 
	
    /**
     * No default constructor.
     */
    protected ManualMatchingPlugin() {
    		cache = null;
        // no default constructor allowed here
    }

    /**
     * Plugin should be constructed with the flag. The flag is used to search
     * only in certain "type" of manually matched items. For example bodies,
     * tenders etc.
     *
     * @param flag
     *            flag/type used to identify item
     * @param dao
     *           dao providing access to manual matches 
     */
    public ManualMatchingPlugin(final ManualMatchDAO dao, final String flag) {
        this.manualMatchDao = dao;
        this.flag = flag;
        cache = new Cache2kBuilder<String, String>() {}
			.name("manualMatchCache_" + flag)
			.eternal(true)
			.entryCapacity(20000000)
			.build();
        populateCache();
    }

	@Override
    public final MatchingResult match(final T item) {
        MatchingResult matchingResult = new MatchingResult();
//        logger.i("Number of matched groups whitch includes etalon is greather then 1. {}", groupIds);
        String groupId = cache.peek(item.getFullHash());
        if (groupId != null) {
            // the same hash found, storing into the same group
            matchingResult.setGroupId(groupId);
            matchingResult.setMatchedBy(MATCHED_BY);
            matchingResult.setMatched(true);
        }

        return matchingResult;
    }
	
	/**
	 * Populates cache with already stored results.
	 */
	private void populateCache() {
        List<ManualMatch> manuallyEntered = manualMatchDao.getAllEntries(this.flag);
        if (manuallyEntered != null && !manuallyEntered.isEmpty()) {
        		for (ManualMatch match : manuallyEntered) {
        			cache.put(match.getFullHash(), match.getGroupId());
        		}
        }
	}
}
