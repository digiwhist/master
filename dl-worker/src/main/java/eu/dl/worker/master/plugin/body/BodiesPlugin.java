package eu.dl.worker.master.plugin.body;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.utils.InitUtils;
import eu.dl.dataaccess.utils.BodyUtils;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.master.plugin.generic.UnionPlugin;
import eu.dl.worker.master.plugin.generic.converter.TenderConverter;
import eu.dl.worker.utils.BasePlugin;
import org.apache.commons.lang3.StringUtils;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * This plugin masters arrays of bodies. If there is more than one body on some array, creates union, if not, uses
 * last published body.
 *
 * @param <T>
 *         matched items type
 * @param <V>
 *         item type to be mastered
 * @param <U>
 *         list of root items
 */
public class BodiesPlugin<T extends MasterablePart, V, U> extends BasePlugin implements MasterPlugin<T, V, U> {

    /**
     * Plugin name.
     */
    public static final String PLUGIN_ID = "bodiesPlugin";

    private List<String> fieldNames;

    /**
     * No empty constructor allowed.
     */
    protected BodiesPlugin() {
        // no empty constructor allowed.
    }

    /**
     * Initializes the plugin with field names to be mastered. The plugin will
     * call getFieldName and setFieldName methods on items in the order defined
     * by comparator.
     *
     * @param fieldNames
     *         field name to be mastered
     */
    public BodiesPlugin(final List<String> fieldNames) {
        super();
        this.fieldNames = fieldNames.stream().map(StringUtils::capitalize).collect(Collectors.toList());
    }

    /**
     * Initializes the plugin with field names list to be mastered. The plugin
     * will call getFieldName and setFieldName methods on items in the order
     * defined by comparator.
     *
     * @param fieldName
     *         field name to be cleaned
     */
    public BodiesPlugin(final String fieldName) {
        this(Arrays.asList(fieldName));
    }


    @Override
    public final V master(final List<T> items, final V finalItem, final List<U> context) {
        for (String fieldName : fieldNames) {
            try {
                // getter method
                Method getter = items.get(0).getClass().getMethod("get" + fieldName);

                boolean unionMade = false;

                // Go through arrays of bodies and find if some contains more than one, if yes create union
                for (T item : items) {
                    List<MatchedBody> bodies = (List<MatchedBody>) getter.invoke(item);

                    // check whether there is at least one item with more then one body
                    if (bodies != null && bodies.size() > 1) {
                        // item with more bodies found, lets union them
                        MasterPlugin masterPlugin = new UnionPlugin(fieldName, new TenderConverter());
                        masterPlugin.master(items, finalItem, context);
                        unionMade = true;
                        // break out the cycle, the union is done already
                        break;
                    }
                }

                // If union was not made, take the most complete body
                if (!unionMade) {
                    List<MatchedBody> bodies = new ArrayList<MatchedBody>();

                    // collect bodies from all items to one list
                    for (T item : items) {
                        if ((List<MatchedBody>) getter.invoke(item) != null) {
                            bodies.addAll((List<MatchedBody>) getter.invoke(item));
                        }
                    }

                    // check, whether there are at least some bodies
                    if (!bodies.isEmpty()) {
                        // pick up the most complete body
                        List<MasterBody> masterBodies = InitUtils
                                .matchedToMasterBody(Arrays.asList(BodyUtils.getMostCompleteBody(bodies)));

                        // search for proper setter method
                        for (Method method : finalItem.getClass().getMethods()) {                        
                            if (method.getName().equals("set" + fieldName)) {
                                // store the result
                                method.invoke(finalItem, masterBodies);
    
                                // skip the loop, the value is already set
                                break;
                            }
                        }
                    }
                }
            } catch (Exception e) {
                // unable to pick the last value
                logger.error("Unable to pick the last value for field '{}' with exception {}", fieldName, e);
                throw new UnrecoverableException("Unable to pick value for exception", e);
            }
        }
        return finalItem;
    }
}
