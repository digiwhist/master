package eu.dl.worker.master;

import java.util.Arrays;
import java.util.List;

import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.worker.master.plugin.AddressPlugin;
import eu.dl.worker.master.plugin.body.BodyIdPlugin;
import eu.dl.worker.master.plugin.body.MetaDataPlugin;
import eu.dl.worker.master.plugin.generic.LogicalORPlugin;
import eu.dl.worker.master.plugin.generic.ModusPlugin;
import eu.dl.worker.master.plugin.generic.UnionPlugin;
import eu.dl.worker.master.plugin.generic.converter.TenderConverter;

/**
 * Common functionality for all Body Master Record Deduplicators.
 *
 * @param <T>
 *            body type to be used
 * @param <V>
 *            body type to be used
 */
public abstract class BaseBodyMaster<T extends MatchedBody, V extends MasterBody> extends BaseMaster<T, V> {

    @Override
    protected final void registerCommonPlugins() {
        pluginRegistry
                .registerPlugin("bodyId", new BodyIdPlugin())
                .registerPlugin("mainActivities", new UnionPlugin<>(Arrays.asList("mainActivities"),
                        new TenderConverter()))
                .registerPlugin("contactsAndType", new ModusPlugin<>(
                        Arrays.asList("name", "email", "contactPoint", "contactName", "phone", "buyerType"),
                        new TenderConverter()))
                .registerPlugin("address", new AddressPlugin<>(Arrays.asList("address")))
                .registerPlugin("fields", new LogicalORPlugin<>(
                        Arrays.asList("isPublic", "isSubsidized", "isSectoral", "isSme")))
                .registerPlugin("metaData", new MetaDataPlugin());
        logger.debug("Registered body master common plugins to registry.");
    }

    @Override
    protected final List<T> generalPreprocessData(final List<T> items) {
        return items;
    }
    
    @Override
    protected final String getPersistentId(final List<T> matchedItems) {
        // there is no persistent id in use
        return null;
    }
    
    @Override
    protected final V postProcessMasterRecord(final V body,
            final List<T> matchedBodies) {
        return body;
    }
}
