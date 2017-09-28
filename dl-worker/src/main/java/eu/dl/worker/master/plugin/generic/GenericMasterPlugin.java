package eu.dl.worker.master.plugin.generic;

import eu.dl.worker.utils.BasePlugin;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Ancestor of generic master plugins providing functionality fommon for such plugins.
 */
public abstract class GenericMasterPlugin extends BasePlugin {
    protected List<String> fieldNames;

    /**
     * Initializes field names for usage.
     *
     * @param fieldNames
     *      name of mastered fields
     */
    protected GenericMasterPlugin(final List<String> fieldNames) {
        super();
        this.fieldNames = fieldNames.stream().map(StringUtils::capitalize).collect(Collectors.toList());
    }
}
