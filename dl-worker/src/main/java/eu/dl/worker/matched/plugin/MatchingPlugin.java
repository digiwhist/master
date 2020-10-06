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
}
