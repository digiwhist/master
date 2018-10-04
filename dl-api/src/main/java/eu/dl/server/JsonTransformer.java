package eu.dl.server;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import eu.dl.dataaccess.annotation.SystemProperty;
import eu.dl.dataaccess.annotation.Transformable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spark.ResponseTransformer;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.List;

/**
 * Transformer of all the objects into JSON based on Jackson library.
 *
 * @author Kuba Krafka
 */
public class JsonTransformer implements ResponseTransformer {

    private final ObjectMapper mapper;

    private static final Logger logger = LoggerFactory.getLogger(JsonTransformer.class);
    
    /**
     * Initialisation of the transformer.
     */
    public JsonTransformer() {
        mapper = new ObjectMapper();
        mapper.registerModule(new JavaTimeModule());
        mapper.setDateFormat(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"));
        mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        mapper.configure(SerializationFeature.WRITE_NULL_MAP_VALUES, false);
    }


    /**
     * Preprocesses data before rendering to JSON.
     *
     * @param data
     *      transformed data
     */
    private void preprocessData(final Object data) {
        clean(data);
    }

    @Override
    public final String render(final Object data) {
        try {
            if (data instanceof List) {
                ((List<Object>) data).forEach(i -> { preprocessData(i); });
            } else {
                preprocessData(data);
            }

            return mapper.writeValueAsString(data);
        } catch (final JsonProcessingException e) {
            throw new RuntimeException("Unable to serialize message", e);
        }
    }

    /**
     * Cleans object based on annotations.
     * 
     * @param object object to be cleaned
     */
    private static void clean(final Object object) {
        if (object == null || !object.getClass().isAnnotationPresent(Transformable.class)) {
            return;
        }

        try {
            Class clazz = object.getClass();
            while (clazz != null) {
                for (Method method : clazz.getMethods()) {
                    // if method is annotated with @SystemProperty, invoke setter with null
                    if (method.isAnnotationPresent(SystemProperty.class) && method.getParameterCount() == 1) {
                        try {
                            method.invoke(object, new Object[]{null});
                        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                            logger.error("Unable to transform result", e);
                            throw new RuntimeException("Unable to transform result", e);
                        }
                    }

                    // if getter is found, clean the result as well
                    if (method.getName().startsWith("get") && method.getParameterCount() == 0
                            && !method.isAnnotationPresent(JsonIgnore.class)) {
                        logger.trace("Cleaning method {} of {}", method.getName(), clazz.getName());
                        try {
                            Object result = method.invoke(object);
                            if (result != null) {
                                if (result instanceof Collection<?>) {
                                    ((Collection) result).forEach(i -> {
                                        clean(i);
                                    });
                                } else {
                                    clean(result);
                                }
                            }
                        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                            logger.error("Unable to transform result", e);
                            throw new RuntimeException("Unable to transform result", e);
                        }
                    }
                }

                clazz = clazz.getSuperclass();
            }
        } catch (Throwable e) {
            logger.error("Unable to transform result", e);
        }
    }
}
