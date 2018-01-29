package eu.dl.dataaccess.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import eu.dl.dataaccess.dto.clean.CleanBody;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.generic.Address;

/**
 * Tests for ClassUtils class.
 *
 * @author Tomas Mrazek
 */
public final class ClassUtilsTest {
    /**
     * Example of the list of empty items.
     */
    private static final List<BodyIdentifier> EMPTY_BODY_IDS = new ArrayList<>(Arrays.asList(
        new BodyIdentifier().setScope(BodyIdentifier.Scope.CZ),
        new BodyIdentifier().setType(BodyIdentifier.Type.ORGANIZATION_ID)));

    /**
     * Example of the empty object.
     */
    private static final CleanBody EMPTY_BODY = new CleanBody()
        .setAddress(new Address())
        .setBodyIds(EMPTY_BODY_IDS)
        .addMainActivity(null);

    /**
     * Example of the non-empty object.
     */
    private static final CleanBody NONEMTPY_BODY = new CleanBody()
        .setAddress(new Address().setStreet("street"))
        .addBodyId(new BodyIdentifier().setId("12345678"))
        .addMainActivity(null)
        .getValid();



    /**
     * Test of {@link ClassUtils#removeNonsenses(java.lang.Object)} and
     * {@link ClassUtils#removeNonsenses(java.util.List)}.
     */
    @Test
    public void removeNonsences() {
        assertNull(ClassUtils.removeNonsenses(EMPTY_BODY_IDS));

        assertNull(ClassUtils.removeNonsenses(EMPTY_BODY));

        List<Address> addrs = ClassUtils.removeNonsenses(new ArrayList<>(Arrays.asList(
            new Address(), null, new Address().setCity("city"))));
        assertEquals(1, addrs.size());
        assertEquals("city", addrs.get(0).getCity());
    }

    /**
     * Test of {@link Validable#getValid()}.
     */
    @Test
    public void getValidTest() {
        assertNull(EMPTY_BODY.getValid());

        CleanBody body = NONEMTPY_BODY.getValid();

        assertTrue(body instanceof CleanBody);

        assertTrue(body.getAddress() instanceof Address);
        assertEquals("street", body.getAddress().getStreet());

        assertEquals(1, body.getBodyIds().size());
        assertEquals("12345678", body.getBodyIds().get(0).getId());

        assertNull(body.getMainActivities());
    }

    /**
     * Test of {@link ClassUtils#getProperty(java.lang.Object, java.lang.String)}.
     */
    @Test
    public void getPropertyTest() {
        assertEquals("street", ClassUtils.getProperty(NONEMTPY_BODY, "address.street"));
        assertEquals("12345678", ClassUtils.getProperty(NONEMTPY_BODY, "bodyIds.0.id"));
        assertNull(ClassUtils.getProperty(NONEMTPY_BODY, "name"));

        assertNull(ClassUtils.getProperty(NONEMTPY_BODY, "address.unknown"));
        assertNull(ClassUtils.getProperty(null, "name"));
        assertNull(ClassUtils.getProperty(NONEMTPY_BODY, null));
        // index is out of range
        assertNull(ClassUtils.getProperty(NONEMTPY_BODY, "bodyIds.1.id"));
        // bodyIds is an list, so the number index is expected. 'a' isn't number so null is returned.
        assertNull(ClassUtils.getProperty(NONEMTPY_BODY, "bodyIds.a.id"));
    }
}
