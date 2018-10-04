/**
 *
 */
package eu.dl.worker;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.databind.type.TypeFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import eu.dl.core.UnrecoverableException;
import java.io.IOException;

/**
 * Represents default message with fluent interface. Method to string is able to
 * serialize the message into json to be sent.
 *
 * @author Kuba Krafka
 */
public class SimpleMessage implements Message {

    private static final String METADATA_KEY = "8hfamudahf987t342thlhjdsnfjkln9qe8uuth9qgbhda";

    private HashMap<String, Object> data = new HashMap<String, Object>();

    private final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    private final ObjectMapper mapper;

    /**
     * Public Constructor.
     */
    public SimpleMessage() {
        super();
        mapper = new ObjectMapper();
        // java time module must be registered to support java 8 date time classes
        mapper.registerModule(new JavaTimeModule());
    }

    @Override
    public final Message setValue(final String key, final String value) {
        data.put(key, value);
        return this;
    }

    @Override
    public final String toJson() {
        try {
            return mapper.writeValueAsString(data);
        } catch (final JsonProcessingException e) {
            logger.error("Unable to serialize message to be send");
            throw new UnrecoverableException("Unable to serialize message", e);
        }
    }

    @Override
    public final String getValue(final String key) {
        return (String) data.get(key);
    }

    @Override
    public final void init(final String json) {
        try {
            data = mapper.readValue(json, new TypeReference<Map<String, Object>>() {
            });
        } catch (final Exception ex) {
            logger.error("Unable to create from json", ex);
            logger.error("Message body: {}", json);
            throw new UnrecoverableException("Unable to create message from provided json", ex);
        }
    }

    @Override
    public final Message setMetaData(final HashMap<String, Object> metaData) {
        data.put(METADATA_KEY, metaData);
        return this;
    }

    @SuppressWarnings("unchecked")
    @Override
    public final HashMap<String, Object> getMetaData() {
        return (HashMap<String, Object>) data.get(METADATA_KEY);
    }

    @Override
    public final <T> T getValueAsObject(final String key, final Class<T> cls) {
        if (key == null || cls == null) {
            return null;
        }
        
        String json = null;
        try {
            json = mapper.writeValueAsString(data.get(key));
            return (T) mapper.readValue(json, cls);
        } catch (JsonProcessingException ex) {
            logger.error("Unable to create json from {}", data.get(key), ex);
            throw new UnrecoverableException("Unable to create json from message property", ex);
        } catch (IOException ex) {
            logger.error("Unable to create {} instance from json {}", cls.getName(), json, ex);
            throw new UnrecoverableException("Unable to create new instance from message property", ex);
        }
    }

    @Override
    public final <T> List<T> getValueAsList(final String key, final Class<T> cls) {
        if (key == null || cls == null) {
            return null;
        }

        String json = null;
        try {
            json = mapper.writeValueAsString(data.get(key));
            return mapper.readValue(json, TypeFactory.defaultInstance().constructCollectionType(List.class, cls));
        } catch (JsonProcessingException ex) {
            logger.error("Unable to create json from {}", data.get(key), ex);
            throw new UnrecoverableException("Unable to create json from message property", ex);
        } catch (IOException ex) {
            logger.error("Unable to create list of {} instances from json {}", cls.getName(), json, ex);
            throw new UnrecoverableException("Unable to create list from message property", ex);
        }
    }
}
