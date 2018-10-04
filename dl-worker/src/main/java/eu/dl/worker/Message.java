package eu.dl.worker;

import java.util.HashMap;
import java.util.List;

/**
 * Message send between workers to do some work.
 *
 * @author Kuba Krafka
 */
public interface Message {

    /**
     * Create message and populates with data from json string.
     *
     * @param json
     *            message
     */
    void init(String json);

    /**
     * set the parama of message to be used and returns the message instance,
     * which is handy for fluent interface.
     *
     * @param key
     *            key
     * @param value
     *            value
     *
     * @return instance of this message to be used in fluent interface
     */
    Message setValue(String key, String value);

    /**
     * Return the value of param identified by key.
     *
     * @param key
     *            key
     * @return value of param
     */
    String getValue(String key);

    /**
     * Sets the message specific meta data.
     *
     * @param metaData
     *            message meta data
     *
     * @return instance of this message to be used in fluent interface
     */
    Message setMetaData(HashMap<String, Object> metaData);

    /**
     * Returns the message specific meta data.
     *
     * @return meta data
     */
    HashMap<String, Object> getMetaData();

    /**
     * Returns json interpretation of the message.
     *
     * @return json
     */
    String toJson();

    /**
     * Returns param identified by key as an instance of T class.
     *
     * @param <T>
     *      class of returned object
     * @param key
     *      key
     * @param cls
     *      class of the returned object
     * @return instance of the given class
     */
    <T> T getValueAsObject(String key, Class<T> cls);

    /**
     * Return param identified by key as a list of T class instances.
     *
     * @param <T>
     *      class of item of returned list
     * @param key
     *            key
     * @param cls
     *      class of the returned object
     * @return value of param
     */
    <T> List<T> getValueAsList(String key, Class<T> cls);
}
