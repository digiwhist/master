package eu.digiwhist.server;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import eu.dl.dataaccess.annotation.SystemProperty;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.master.MasterTender;
import spark.ResponseTransformer;

/**
 * Simple transformer of all the objects into JSON.
 *
 * @author Kuba Krafka
 */
public final class JsonTransformer implements ResponseTransformer {

    private ObjectMapper mapper;

    private static final Logger logger = LoggerFactory.getLogger(JsonTransformer.class);
    
    /**
     * Initialisation of the transformer.
     */
    public JsonTransformer() {
        mapper = new ObjectMapper();
        mapper.registerModule(new JavaTimeModule());
        mapper.setDateFormat(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"));
        mapper.setSerializationInclusion(Include.NON_NULL);
        mapper.configure(SerializationFeature.WRITE_NULL_MAP_VALUES, false);
    }

    @Override
    public String render(final Object data) {
        try {
        		for (Object item: (List<Object>) data) {
        			((MasterTender) item).setGroupId(null);
        			clean(item);
        		}
            return mapper.writeValueAsString(data);
        } catch (final JsonProcessingException e) {
            throw new RuntimeException("Unable to serialize message", e);
        }
    }

    /**
     * Cleans object based on annotations.
     * @param object object to be cleaned
     */
    private static void clean(final Object object) {
    		if (object == null || !object.getClass().isAnnotationPresent(Transformable.class)) {
    			return;
    		}
    		
    		Boolean hasSuperClass = true;
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
		    				&& !method.getName().startsWith("getValid")) {
		    			try {
						Object result = method.invoke(object);
						if (result != null) {
							if (result instanceof Collection<?>) {
								for (Object item : (Collection) result) {	
									clean(item);
								}
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
    }
}
