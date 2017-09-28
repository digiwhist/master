package eu.dl.worker.master.plugin.body;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier.Scope;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.matched.EtalonBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.utils.BasePlugin;
import org.apache.commons.collections.map.MultiKeyMap;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

/**
 * This plugin is used to master bodyIds.
 *
 * @param <T>
 *            matched items type
 * @param <V>
 *            item type to be mastered
 */
public final class BodyIdPlugin<T extends MatchedBody, V> extends BasePlugin implements MasterPlugin<T, V, Object> {
    /**
     * Plugin name.
     */
    public static final String PLUGIN_ID = "bodyIdPlugin";

    @Override
    public V master(final List<T> items, final V finalItem, final List<Object> context) {
        // lets iterate over bodies and kind of sort all ids into groups of type
        // and scope
        MultiKeyMap storage = new MultiKeyMap();
        
        for (MatchedBody body : items) {
            List<BodyIdentifier> ids = body.getBodyIds();
            
            // additional infos about body
            Boolean etalon = false;
            if (body.getSource() != null && body.getSource().equals(EtalonBody.ETALON_SOURCE_ID.toString())) {
                etalon = true;
            }

            LocalDate publicationDate = body.getPublicationDate();

            if (ids != null) {                
                for (BodyIdentifier id : ids) {
                    if (id.getId() != null) {
                        // create enhanced id with the info about etalon and
                        // publication date
                        EnhancedBodyIdentifier enhancedId = new EnhancedBodyIdentifier(id, etalon,
                                publicationDate);
                        
                        Scope scope = id.getScope();
                        BodyIdentifier.Type type = id.getType();
                        
                        if (storage.get(scope, type) != null) {
                            // list already exists
                            List<EnhancedBodyIdentifier> list = 
                                    (List<EnhancedBodyIdentifier>) storage.get(scope, type);
                            
                            // add new item
                            list.add(enhancedId);
                            storage.put(scope, type, list);
                        } else {
                            ArrayList<EnhancedBodyIdentifier> list = new ArrayList<EnhancedBodyIdentifier>();
                            list.add(enhancedId);
                            storage.put(scope, type, list);
                        }
                    }
                }
            }
        }
        
        List<BodyIdentifier> result = new ArrayList<BodyIdentifier>();

        // iterate over sorted storage and evaluate winners
        for (Object entry : storage.entrySet()) {
            Object value = ((Entry) entry).getValue();
            List<EnhancedBodyIdentifier> list = (List<EnhancedBodyIdentifier>) value;
            result.add(evaulate(list));
        }

        MasterBody masterBody = (MasterBody) finalItem;
        
        if (!result.isEmpty()) {
            masterBody.setBodyIds(result);
        }
        
        return finalItem;
    }

    /**
     * Method which pick ups the proper candidate from the list of candidates.
     * 
     * @param bodyIdentifierList
     *            list of identifier candidates
     * @return the body_id candidate
     */
    private BodyIdentifier evaulate(final List<EnhancedBodyIdentifier> bodyIdentifierList) {
        if (bodyIdentifierList == null || bodyIdentifierList.isEmpty()) {
            return null;
        } else if (bodyIdentifierList.size() == 1) {
            // only one candidate
            return cleanMetaData(bodyIdentifierList.get(0));
        } else {
            HashMap<String, Integer> counts = new HashMap<String, Integer>();
            
            // iterate over all ids to count value usage
            for (EnhancedBodyIdentifier id : bodyIdentifierList) {
               if (id.getEtalon()) {
                   // if there is etalon entry, return that as preffered
                   return id;
               }
               
               // count value
               if (counts.get(id.getId()) != null) {
                   // increment counter
                   counts.put(id.getId(), counts.get(id.getId()) + 1);
               } else {
                   // first occurrence
                   counts.put(id.getId(), 1);
               }
            }

            // no etalon found, lets continue
            // get most often value
            Integer maxCount = 0;
            ArrayList<String> values = new ArrayList<String>();

            for (Entry<String, Integer> entry : counts.entrySet()) {
                if (entry.getValue() > maxCount) {
                    // new leader
                    // reset storage of most common values
                    values = new ArrayList<String>();
                    values.add(entry.getKey());
                    maxCount = entry.getValue();
                } else if (entry.getValue() == maxCount) {
                    // more the one value is the most often
                    values.add(entry.getKey());
                }
            }
            
            // iterate over ids to pick only most often ones
            EnhancedBodyIdentifier mostOften = null;
            LocalDate newestDate = LocalDate.MIN;

            for (EnhancedBodyIdentifier id : bodyIdentifierList) {
                // lets check only candidates from most often value groups
                if (values.contains(id.getId())) {
                    // init the most often for the case there are no publication dates set
                    if (mostOften == null) {
                        mostOften = id;
                    }
                    
                    // compare dates
                    if (id.getPublicationDate() != null && id.getPublicationDate().isAfter(newestDate)) {
                        mostOften = id;
                        newestDate = id.getPublicationDate();
                    }
                }
            }
            
            return cleanMetaData(mostOften);
            
        }
    }
    
    /**
     * Removes metadata from the body identifier.
     * 
     * @param id 
     *  remove from this instance
     * @return clean id
     */
    private BodyIdentifier cleanMetaData(final EnhancedBodyIdentifier id) {
        id.setEtalon(null);
        id.setPublicationDate(null);
        return id;
    }
}
