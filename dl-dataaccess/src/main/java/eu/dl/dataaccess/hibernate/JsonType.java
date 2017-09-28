package eu.dl.dataaccess.hibernate;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;

import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.usertype.UserType;

import eu.dl.dataaccess.dto.JsonData;

/**
 * Hibernate type used to store jsonb data.
 *
 */
public class JsonType implements UserType {

    @Override
    public final int[] sqlTypes() {
        return new int[] { Types.JAVA_OBJECT };
    }

    @Override
    public final Class<JsonData> returnedClass() {
        return JsonData.class;
    }

    @Override
    public final Object nullSafeGet(final ResultSet rs, final String[] names,
            final SharedSessionContractImplementor session,
            final Object owner) throws HibernateException, SQLException {
        final String cellContent = rs.getString(names[0]);
        if (cellContent == null) {
            return null;
        }
        try {
            return new JsonData().setData(cellContent);
        } catch (final Exception ex) {
            throw new RuntimeException("Failed to convert String to JsonData: " + ex.getMessage(), ex);
        }
    }

    @Override
    public final void nullSafeSet(final PreparedStatement ps, final Object value, final int idx,
            final SharedSessionContractImplementor session) throws HibernateException, SQLException {
        if (value == null) {
            ps.setNull(idx, Types.OTHER);
            return;
        }
        try {
            ps.setObject(idx, ((JsonData) value).getData(), Types.OTHER);
        } catch (final Exception ex) {
            throw new RuntimeException("Failed to convert JsonData to String: " + ex.getMessage(), ex);
        }
    }

    @Override
    public final Object deepCopy(final Object value) throws HibernateException {
        try {
            // use serialization to create a deep copy
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            ObjectOutputStream oos = new ObjectOutputStream(bos);
            oos.writeObject(value);
            oos.flush();
            oos.close();
            bos.close();

            ByteArrayInputStream bais = new ByteArrayInputStream(bos.toByteArray());
            return new ObjectInputStream(bais).readObject();
        } catch (ClassNotFoundException | IOException ex) {
            throw new HibernateException(ex);
        }
    }

    @Override
    public final boolean isMutable() {
        return true;
    }

    @Override
    public final Serializable disassemble(final Object value) throws HibernateException {
        return (Serializable) this.deepCopy(value);
    }

    @Override
    public final Object assemble(final Serializable cached, final Object owner) throws HibernateException {
        return this.deepCopy(cached);
    }

    @Override
    public final Object replace(final Object original, final Object target, final Object owner)
            throws HibernateException {
        return this.deepCopy(original);
    }

    @Override
    public final boolean equals(final Object obj1, final Object obj2) throws HibernateException {
        if (obj1 == null) {
            return obj2 == null;
        }
        return obj1.equals(obj2);
    }

    @Override
    public final int hashCode(final Object obj) throws HibernateException {
        return obj.hashCode();
    }
}