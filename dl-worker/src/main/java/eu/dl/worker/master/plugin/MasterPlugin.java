package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.matched.MasterablePart;

import java.util.List;

/**
 * Interface for the mastering plugin. The mastering happens in the master
 * action.
 *
 * @param <T>
 *         matched items
 * @param <V>
 *         mastered items
 * @param <U>
 *         type of context items
 */
public interface MasterPlugin<T extends MasterablePart, V, U> {
    /**
     * Takes data from source matched items, masters some fields of them and
     * returns result.
     *
     * @param items
     *         item set to be mastered
     * @param finalItem
     *         mastered item
     * @param context
     *         context of all root items
     *
     * @return mastered item
     */
    V master(List<T> items, V finalItem, List<U> context);
}
