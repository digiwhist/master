package eu.dl.dataaccess.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import eu.dl.dataaccess.dto.generic.Address;

/**
 * Tests for ValidationUtils class.
 * 
 * @author Tomas Mrazek
 */
public final class ValidationUtilsTest {
    /**
     * Test of {@link ValidationUtils#getValid(java.lang.Object, java.lang.Object...)} and
     * {@link ValidationUtils#getValid(java.util.List)}.
     */
    @Test
    public void getValidTest() {
        Address adr = new Address();
        adr.setState("state");
        // this will be null although state is not null, because of decision about validity is based on city, country,
        // and nuts fileds.
        assertNull(ValidationUtils.getValid(adr, adr.getCity(), adr.getCountry(), adr.getNuts()));

        
        adr.setCity("city");
        // now, the address is valid because of one of tested parameters is not null (city in this case).
        Address validAddr = ValidationUtils.getValid(adr, adr.getCity(), adr.getCountry(), adr.getNuts());
        assertTrue(validAddr instanceof Address);
        assertEquals("city", validAddr.getCity());


        // is necessary to use inmutable list
        List<String> list = new ArrayList<>();
        list.add(null);
        list.add("");
        // this will be null, list includes only empty elements
        assertNull(ValidationUtils.getValid(list));

        
        list.add("string");
        // now, the list is valid because includes at least non-empty element
        List<String> validList = ValidationUtils.getValid(list);
        assertTrue(validList instanceof List);
        assertEquals(1, validList.size());
        assertEquals("string", validList.get(0));
    }

}
