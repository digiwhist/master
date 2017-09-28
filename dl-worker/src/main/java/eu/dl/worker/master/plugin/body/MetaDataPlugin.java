package eu.dl.worker.master.plugin.body;

import java.util.HashMap;
import java.util.List;

import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.utils.BasePlugin;

/**
 * This plugin is used to master data for bodies.
 *
 * @param <T>
 *            matched items type
 * @param <V>
 *            item type to be mastered
 */
public final class MetaDataPlugin<T extends MatchedBody, V> extends BasePlugin implements MasterPlugin<T, V, Object> {
    /**
     * Plugin name.
     */
    public static final String PLUGIN_ID = "metaDataPlugin";

    @Override
    public V master(final List<T> items, final V finalItem, final List<Object> context) {
        HashMap<String, Object> result = new HashMap<String, Object>();
        
        for (MatchedBody body : items) {
        		if (body.getMetaData() != null) {
        			result.putAll(body.getMetaData());
        		}
        }

        MasterBody masterBody = (MasterBody) finalItem;
        
        if (!result.isEmpty()) {
            masterBody.setMetaData(result);
        }
        
        return finalItem;
    }


}
