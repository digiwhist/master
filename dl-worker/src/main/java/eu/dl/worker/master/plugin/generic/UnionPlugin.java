package eu.dl.worker.master.plugin.generic;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.generic.Payment;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.utils.DTOUtils;
import eu.dl.dataaccess.utils.BodyUtils;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.master.plugin.generic.converter.Converter;
import eu.dl.worker.utils.ArrayUtils;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

/**
 * This plugin creates union of multiple arrays.
 *
 * @param <T>
 *           matched items type
 * @param <V>
 *           item type to be mastered
 * @param <U>
 *         context items type
 */
public final class UnionPlugin<T extends MasterablePart, V, U>
        extends GenericMasterPlugin implements MasterPlugin<T, V, U> {

    private Converter converter;


    /**
     * Initializes the plugin with field names to be mastered. The plugin will
     * call getFieldName and setFieldName methods on items in the order defined
     * by comparator.
     *
     * @param fieldNames field name to be mastered
     *
     * @param converter
     *         used to convert values if needed
     */
    public UnionPlugin(final List<String> fieldNames, final Converter converter) {
        super(fieldNames);
        this.converter = converter;
    }

    /**
     * Initializes the plugin with field names to be mastered. The plugin will
     * call getFieldName and setFieldName methods on items in the order defined
     * by comparator.
     *
     * @param fieldName
     *         field name to be mastered
     * @param converter
     *         used to convert values if needed
     */
    public UnionPlugin(final String fieldName, final Converter converter) {
        this(Arrays.asList(fieldName), converter);
    }

    @Override
    public V master(final List<T> items, final V finalItem, final List<U> context) {
        for (String fieldName : fieldNames) {
            try {
                // getter method
                Method getter = items.get(0).getClass().getMethod("get" + fieldName);

                // Get content of all lists
                List<Object> listOfAll = new ArrayList<>();
                for (Object item : items) {
                    final Object temp = getter.invoke(item);

                    if (temp != null) {
                        listOfAll.addAll((List<Object>) temp);
                    }
                }

                List<Object> result;
                // Get rid of duplicates
                // Publications
                if (!listOfAll.isEmpty() && listOfAll.get(0).getClass().equals(Publication.class)) {
                    result = listOfAll.stream().filter(ArrayUtils.distinct(p ->
                                    ((Publication) p).getSourceId() +
                                    ((Publication) p).getMachineReadableUrl() +
                                    ((Publication) p).getHumanReadableUrl() +
                                    ((Publication) p).getPublicationDate() +
                                    ((Publication) p).getVersion() +
                                    ((Publication) p).getIsIncluded())).collect(Collectors.toList());
                    HashMap<String, Object> isIncludedPreferred = new HashMap<>();

                    for (Object publication : result) {
                        String key = ((Publication) publication).getSourceId() +
                                    ((Publication) publication).getMachineReadableUrl() +
                                    ((Publication) publication).getHumanReadableUrl() +
                                    ((Publication) publication).getPublicationDate() +
                                    ((Publication) publication).getVersion();
                        if (!isIncludedPreferred.containsKey(key)
                                || (isIncludedPreferred.containsKey(key)
                                && ((Publication) publication).getIsIncluded() != null
                                && ((Publication) publication).getIsIncluded())) {
                            isIncludedPreferred.put(key, publication);
                        }
                    }

                    result = new ArrayList<>(isIncludedPreferred.values());
                // Bidders
                } else if (BodyUtils.getBodyFieldNamesInLowerCase().contains(fieldName.toLowerCase())) {
                    result = listOfAll.stream().filter(ArrayUtils.distinct(t -> ((MatchedBody) t).getGroupId()))
                            .collect(Collectors.toList());
                // Payments
                } else if (fieldName.toLowerCase().equals("payments")) {
                    result = listOfAll.stream().filter(t -> {
                        if (((Payment) t).getPaymentDate() == null) {
                            return true;
                        }

                        return ArrayUtils.distinct(tt -> ((Payment) tt).getPaymentDate()).test(t);
                    }).collect(Collectors.toList());
                // BodyIds
                } else if (fieldName.toLowerCase().equals("bodyids")) {
                    result = listOfAll
                            .stream()
                            .map(t -> (BodyIdentifier) t)
                            .filter(ArrayUtils.distinct(t ->
                                    (t.getId() == null ? "" : t.getId())
                                    .concat(t.getType() == null ? "" : t.getType().toString())
                                    .concat(t.getScope() == null ? "" : t.getScope().toString())))
                            .collect(Collectors.toList());
                } else {
                    result = listOfAll.stream().filter(ArrayUtils.distinct())
                        .collect(Collectors.toList());
                }

                // Save the result
                if (!DTOUtils.isEmpty(result)) {
                    for (Method method : finalItem.getClass().getMethods()) {
                        if (method.getName().equals("set" + fieldName)) {
                            method.invoke(finalItem, converter.convert(result));
                            break;
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
