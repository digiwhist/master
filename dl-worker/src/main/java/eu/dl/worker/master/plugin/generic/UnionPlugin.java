package eu.dl.worker.master.plugin.generic;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.generic.Payment;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.generic.UnitPrice;
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
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

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
                    List<Publication> publications = new ArrayList<>();
                    listOfAll.stream().map(n -> (Publication) n)
                        .forEach(n -> {
                            int i = getPublicationIndex(n, publications);
                            // publication isn't in result list, add it
                            if (i < 0) {
                                publications.add(n);
                            // publication is in result list, replace existing with new one in case of included publication
                            } else if (Boolean.TRUE.equals(n.getIsIncluded())) {
                                publications.set(i, n);
                            }
                        });

                    result = new ArrayList<>(publications);
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
                } else if (fieldName.toLowerCase().equals("unitprices")) {
                    result = listOfAll
                        .stream()
                        .map(t -> (UnitPrice) t)
                        .filter(ArrayUtils.distinct(t -> t.getDescription() + t.getUnitNumber()))
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

    /**
     * @param o1
     *      first object to be compared
     * @param o2
     *      second object to be compared
     * @return TRUE if both objects are equal or one of then is NULL, otherwise false.
     */
    private static boolean isEqual(final Object o1, final Object o2) {
        if (o1 == null || o2 == null) {
            return true;
        }

        return o1.equals(o2);
    }

    /**
     * @param p1
     *      first publication to be compared
     * @param p2
     *      second publication to be compared
     * @return true if the publication are same
     */
    private static boolean isPublicationEqual(final Publication p1, final Publication p2) {
        return isEqual(p1.getSourceId(), p2.getSourceId())
            && isEqual(p1.getMachineReadableUrl(), p2.getMachineReadableUrl())
            && isEqual(p1.getHumanReadableUrl(), p2.getHumanReadableUrl())
            && isEqual(p1.getPublicationDate(), p2.getPublicationDate())
            && isEqual(p1.getVersion(), p2.getVersion());
    }

    /**
     * @param publication
     *      publication to be checked
     * @param list
     *      list of publication
     * @return index of an existing publication or -1
     */
    private static int getPublicationIndex(final Publication publication, final List<Publication> list) {
        if (publication == null || list == null || list.isEmpty()) {
            return -1;
        }

        // find publication with callback
        return IntStream.range(0, list.size())
            .filter(i -> isPublicationEqual(publication, list.get(i)))
            .findFirst().orElse(-1);
    }
}
