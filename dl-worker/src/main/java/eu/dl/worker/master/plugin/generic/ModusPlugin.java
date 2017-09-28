package eu.dl.worker.master.plugin.generic;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.utils.DTOUtils;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.master.plugin.generic.converter.Converter;

import java.lang.reflect.Method;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * This plugin takes the object with latest modus value.
 *
 * @param <T>
 *         matched items type
 * @param <V>
 *         item type to be mastered
 * @param <U>
 *         context items type
 */
public class ModusPlugin<T extends MasterablePart, V, U> extends GenericMasterPlugin implements MasterPlugin<T, V, U> {

    private Converter converter;

    /**
     * Initializes the plugin with field names to be mastered. The plugin will
     * call getFieldName and setFieldName methods on items in the order defined
     * by comparator.
     *
     * @param fieldNames
     *         field name to be mastered
     * @param converter
     *         used to convert values if needed
     */
    public ModusPlugin(final List<String> fieldNames, final Converter converter) {
        super(fieldNames);
        this.converter = converter;
    }

    /**
     * Initializes the plugin with field names list to be mastered. The plugin
     * will call getFieldName and setFieldName methods on items in the order
     * defined by comparator.
     *
     * @param fieldName
     *         field name to be cleaned
     * @param converter
     *         used to convert values if needed
     */
    public ModusPlugin(final String fieldName, final Converter converter) {
        this(Arrays.asList(fieldName), converter);
    }

    @Override
    public final V master(final List<T> items, final V finalItem, final List<U> context) {
        for (String fieldName : fieldNames) {
            // getter and setter methods
            try {
                // getter method
                Method getter = items.get(0).getClass().getMethod("get" + fieldName);

                // iterate over all items and store the values into storages
                HashMap<String, Integer> occurrencesCountStorage = new HashMap<>();
                HashMap<String, LocalDate> lastPublicationDateStorage = new HashMap<>();
                
                for (T item : items) {
                    putToOccurrencesCountStorage(getter.invoke(item), occurrencesCountStorage);
                    putToLastPublicationDateStorage(item, getHash(getter.invoke(item)), lastPublicationDateStorage);
                }
                
                if (!occurrencesCountStorage.isEmpty()){
                    // sort items by occurrence, most often first
                    List<Map.Entry<String, Integer>> sortedStorage = occurrencesCountStorage.entrySet().stream()
                            .sorted(Map.Entry.comparingByValue()).collect(Collectors.toList());
                    
                    String winningHash = null;
                    LocalDate winningPublicationDate = null;
                    Integer highestCount = 0;
                    
                    // iterate over all items in the collection and pick the most frequent one
                    // if there are more winners, picks the latest published
                    for (Map.Entry<String, Integer> entry : sortedStorage) {
                        if (entry.getValue().compareTo(highestCount) > 0) {
                            // new most frequent value
                            winningHash = entry.getKey();
                            winningPublicationDate = lastPublicationDateStorage.get(entry.getKey());
                        } else if (entry.getValue().compareTo(highestCount) == 0) {
                            // the same frequency
                            if (winningHash == null) {
                                winningHash = entry.getKey();
                                winningPublicationDate = lastPublicationDateStorage.get(entry.getKey());
                            } else if (lastPublicationDateStorage.get(entry.getKey()) != null) {
                                if (winningPublicationDate == null) {
                                    // there is no publication date for previous entries, 
                                    // this one is winning for now
                                    winningHash = entry.getKey();
                                } else if (lastPublicationDateStorage.get(entry.getKey()).compareTo(
                                        winningPublicationDate) > 0) {
                                    // we have new winner
                                    winningHash = entry.getKey();
                                } 
                            }
                        }
                    }
                    
                    
                    // get the setter used to store value in master item
                    Method setter = null;
                    for (Method methodFound : finalItem.getClass().getMethods()) {
                        if (methodFound.getName().equals("set" + fieldName)) {
                            setter = methodFound;
                            break;
                        }
                    }
                        
                    // iterate over result set and "pick" the first nonempty value
                    for (T item : items) {
                        Object result = getter.invoke(item);

                        // setter method
                        if (!DTOUtils.isEmpty(result) && getHash(result).equals(winningHash)) {
                            // check if we must convert matched to master
                            setter.invoke(finalItem, converter.convert(result));

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
     * Put object to occurrences count storage(hash map) if not there, otherwise increase its counter.
     *
     * @param object object
     * @param occurrencesCountStorage hash
     *
     */
    private void putToOccurrencesCountStorage(
            final Object object, final HashMap<String, Integer> occurrencesCountStorage) {
        if (object == null) {
            return;
        }

        // get unique hash representing the object
        final String objectHash = getHash(object);

        if (!occurrencesCountStorage.containsKey(objectHash)) {
            // first occurrence
            occurrencesCountStorage.put(objectHash, 1);
        } else {
            // item already found, increase counter
            occurrencesCountStorage.put(objectHash, occurrencesCountStorage.get(objectHash) + 1);
        }
    }
    
    
    /**
     * Collects last publication dates of objects with the same hash. 
     *
     * @param object object from which we will try to get lastPublicationDate
     * @param valueHash the hash under which will be the lastPublicationDate stored
     * @param lastPublicationDateStorage hash
     *
     */
    private void putToLastPublicationDateStorage(
            final MasterablePart object,
            final String valueHash, 
            final HashMap<String, LocalDate> lastPublicationDateStorage) {
        if (object == null) {
            return;
        }
       
        // get the publication date
        LocalDate objectPublicationDate = object.getPublicationDate();
        
        if (!lastPublicationDateStorage.containsKey(valueHash)) {
            // first occurrence
            lastPublicationDateStorage.put(valueHash, objectPublicationDate);
        } else if (objectPublicationDate != null) {
            // item already found, increase counter
            if (lastPublicationDateStorage.get(valueHash) == null) {
                // the stored value is null, store new date instead
                lastPublicationDateStorage.put(valueHash, objectPublicationDate);
            } else if (lastPublicationDateStorage.get(valueHash).compareTo(objectPublicationDate) == 1) {
                // newer publication date, lets store that
                lastPublicationDateStorage.put(valueHash, objectPublicationDate);
            }
        }
    }
    
    /**
     * Returns hash used to uniquely identify this value.
     * 
     * @param object key is used for this
     * @return hash
     */
    private String getHash(final Object object) {
        if (object == null) {
            return null;
        }
        
        // get the storage key for this object
        if (object instanceof MatchedBody) {
            // for matched body, the key is its group id 
            return ((MatchedBody) object).getGroupId();
        } else {
            // string is used for others
            return object.toString();
        }
    }
}
