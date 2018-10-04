package eu.datlab.worker.master.plugin;

import eu.dl.dataaccess.dto.generic.Document;
import eu.dl.worker.master.plugin.generic.comparators.DateTimeComparator;
import eu.dl.worker.master.plugin.BaseDocumentPlugin;
import eu.dl.worker.master.plugin.generic.LastPublishedPlugin;
import eu.dl.worker.master.plugin.generic.LastValuePlugin;
import eu.dl.worker.master.plugin.generic.ModusPlugin;
import eu.dl.worker.master.plugin.generic.UnionPlugin;
import eu.dl.worker.master.plugin.generic.converter.TenderConverter;

import java.util.Arrays;

/**
 * Document M&M plugin.
 */
public class TenderDocumentPlugin extends BaseDocumentPlugin {
    @Override
    protected final Object createEmptyListItemInstance() {
        return new Document();
    }

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerNestedMasterPlugins() {
        nestedPluginRegistry
                .registerPlugin("MOD+LNN", new ModusPlugin<>(Arrays.asList(
                        "title", "type", "signatureDate", "version", "order", "language", "url"),
                        new TenderConverter()))
                .registerPlugin("MAX", new LastValuePlugin<>("publicationDateTime",
                        new DateTimeComparator<>("publicationDateTime"), new TenderConverter()))
                .registerPlugin("Union", new UnionPlugin<>(Arrays.asList("otherVersions", "extensions"),
                        new TenderConverter()))
                .registerPlugin("LNN", new LastPublishedPlugin<>(
                        Arrays.asList("description", "format"), new TenderConverter()));
    }
}
