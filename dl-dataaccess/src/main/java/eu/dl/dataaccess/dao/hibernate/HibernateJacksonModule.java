package eu.dl.dataaccess.dao.hibernate;

import com.fasterxml.jackson.databind.module.SimpleModule;

/**
 * Defines mapping for serialization and deserialization.
 */
public final class HibernateJacksonModule extends SimpleModule {

    /**
     *
     */
    private static final long serialVersionUID = 2618344578969921731L;

    /**
     * Initializes new instance with custom JSON serializers and deserializers useful for mapping to and from MongoDb
     * (eg. BSON date).
     */
    protected HibernateJacksonModule() {

    }
}
