package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.worker.utils.BasePlugin;
import eu.dl.worker.utils.BasicPluginRegistry;
import eu.dl.worker.utils.PluginRegistry;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Base plugin class for Match & Master mastering approach used for list variables (eg. lots, bids, etc.).
 *
 * @param <T>
 *         tender (lot) matchable
 * @param <W>
 *         tender masterable
 * @param <U>
 *         lot (bid) matchable
 * @param <X>
 *         lot (bid) masterable
 */
public abstract class
    BaseMatchAndMasterPlugin<T extends MasterablePart, W, U extends MasterablePart, X>
        extends BasePlugin implements MasterPlugin<T, W, U> {
    // plugin registry used for mastering of matched nested items (eg. mastering of matched lots inside tenders)
    protected PluginRegistry<MasterPlugin> nestedPluginRegistry = new BasicPluginRegistry<>();

    /**
     * Class constructor. Registers merging plugins.
     */
    public BaseMatchAndMasterPlugin() {
        super();
        registerNestedMasterPlugins();
    }

    @Override
    public final W master(final List<T> items, final W finalItem, final List<U> context) {
        // get list from each matched input item (eg. list of lots from each matched tender)
        List<List<U>> listsForMatching = getListsForMatching(items);

        // try to match lists items among each other and create lists of matched items
        // (eg. try to match lots and create new lists where each list contains matching lots)
        List<List<U>> matchedLists = match(listsForMatching);

        // for each list with group of matched items, merge items into one master, and return list of master items
        // (eg. for each group of matched lots, create one master lot, and return list of master lots)
        List<X> finalList = masterMatchedLists(matchedLists);

        // set list of master items to final item and return it
        // (eg. set list of master lots to tender and return tenders)
        return setFinalList(finalItem, finalList);
    }

    /**
     * Gets nested lists of items from each item of input matched group (eg. lists of lots from matched tenders)
     *
     * @param items
     *         set of matched input items
     *
     * @return lists from {@code items} to be matched
     */
    // TODO: possibly by reflection (getter)
    protected abstract List<List<U>> getListsForMatching(List<T> items);

    /**
     * Matches items from lists and creates groups of matched items.
     *
     * @param inputListsForMatching
     *         lists with items to be matched (eg. lists of lots)
     *
     * @return lists of matched items (eg. groups of lots) to be mastered later
     */
    protected abstract List<List<U>> match(List<List<U>> inputListsForMatching);

    /**
     * Initializes new empty item that will serve as the master item.
     *
     * @return new empty instance of master item
     */
    protected abstract X createEmptyListItemInstance();

    /**
     * Registers nested plugins to be used for mastering of matched nested items.
     **/
    protected abstract void registerNestedMasterPlugins();

    /**
     * Sets the mastered list of items to parent master item (eg. list of master lots to master tender).
     *
     * @param finalItem
     *         parent master item
     * @param finalList
     *         final list of nested master items
     *
     * @return parent master item with list of nested master items (eg. master tender with list of master lots).
     */
    // TODO: possibly by reflection (setter)
    protected abstract W setFinalList(W finalItem, List<X> finalList);

    /**
     * Sets references (in the form of list of structured ids) on master item to the source (matched) items that the
     * master was created from.
     *
     * @param matchedList
     *         list of source matched items
     * @param masterItem
     *         master item (master record for {@code matchedList})
     *
     * @return master item with references to source matched items
     */
    protected abstract X setSourceStructuredIds(List<U> matchedList, X masterItem);

    /**
     * Takes matched lists and creates master record for each of them.
     *
     * @param matchedLists
     *         lists of matched items to be mastered
     *
     * @return list of master items for each input matched list
     */
    private List<X> masterMatchedLists(final List<List<U>> matchedLists) {
        if (matchedLists == null) {
            return null;
        }

        List<X> finalList = new ArrayList<>();
        for (List<U> matchedList : matchedLists) {
            X masterItem = createEmptyListItemInstance();
            // iterate over all nested mastering plugins and execute them in a proper order
            for (Map.Entry<String, MasterPlugin> entry : nestedPluginRegistry.getPlugins().entrySet()) {
                MasterPlugin<U, X, U> nestedMasterPlugin = entry.getValue();
                masterItem = nestedMasterPlugin.master(matchedList, masterItem, matchedList);
            }
            // set source ids so that it is possible to find which items the master has been created from
            masterItem = setSourceStructuredIds(matchedList, masterItem);

            finalList.add(masterItem);
        }
        return finalList;
    }
}