package eu.dl.worker.matched.plugin;

/**
 * Interface for the matching plugin. The matching happens in the match action.
 * 
 * @param <T>
 *            type of the item to be matched
 * @author Kuba Krafka
 */
public interface MatchingPlugin<T> {
    /**
     * Matches the item against the base of already matched items.
     *
     * @param item
     *            item to be matched
     * 
     * @return result of matching
     */
    MatchingResult match(T item);

    /**
     * Check whether the the given item is matchable by this plugin.
     *
     * @param item
     *      item to be matched
     * @return TRUE if the item is matchable, otherwise FALSE
     */
    boolean isMatchable(T item);
}
