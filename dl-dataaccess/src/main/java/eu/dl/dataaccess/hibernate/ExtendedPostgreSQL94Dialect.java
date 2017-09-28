package eu.dl.dataaccess.hibernate;

import java.sql.Types;

import org.hibernate.dialect.PostgreSQL94Dialect;

/**
 * PostgreSQL dialog with extension to support jsonb.
 *
 */
public class ExtendedPostgreSQL94Dialect extends PostgreSQL94Dialect {

    /**
     * Construct dialect and registers its extensions.
     */
    public ExtendedPostgreSQL94Dialect() {
        super();
        this.registerColumnType(Types.JAVA_OBJECT, "jsonb");
        this.registerHibernateType(Types.OTHER, "eu.dl.dataaccess.hibernate.JsonType");
    }
}