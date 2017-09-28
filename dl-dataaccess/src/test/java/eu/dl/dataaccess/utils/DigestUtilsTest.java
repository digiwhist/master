package eu.dl.dataaccess.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.matched.MatchedBody;

/**
 * Tests for DigestUtils class.
 * 
 * @author Tomas Mrazek
 */
public final class DigestUtilsTest {
    
    /**
     * Test of cases when functions return null.
     */
    @Test
    public void nullTest() {
        assertNull(DigestUtils.standardize(null));
        
        assertNull(DigestUtils.standardizeName(null));
        
        assertNull(DigestUtils.standardizeAddress(null));
        
        
        //not enough informations in address (city is misssing)
        assertNull(DigestUtils.standardizeAddress(new Address().setStreet("Wall Street")));        
        //not enough informations in address (rawAddress is misssing)
        assertNull(DigestUtils.standardizeAddress(new Address().setCountry("US")));
        
        MatchedBody nullMatchedBody = null;
        assertNull(DigestUtils.bodyHash(nullMatchedBody));
        
        assertNull(DigestUtils.digestName(null));
        //name doesn't have enough alphanumeric characters
        assertNull(DigestUtils.digestName("a"));
        
        
        assertNull(DigestUtils.digestAddress(null));
        //street doesn't have enough alphanumeric characters
        assertNull(DigestUtils.digestAddress("a"));
        assertNull(DigestUtils.digestAddress("aa"));
        assertNull(DigestUtils.digestAddress("1"));
        assertNull(DigestUtils.digestAddress("a1"));
        
        
        assertNull(DigestUtils.removeAccents(null));
        

        assertNull(DigestUtils.digest(null, null));
    }
    
    /**
     * Tests of DigestUtils#standardize(java.langString) function.
     */
    @Test
    public void standardizeTest() {
        assertEquals("abc", DigestUtils.standardize("ABc "));
        assertEquals("ab c", DigestUtils.standardize("   AB\n\t c "));
    }
    
    /**
     * Tests of DigestUtils#standardizeName(java.langString) function.
     */
    @Test
    public void standardizeNameTest() {
        assertEquals("name|SR|", DigestUtils.standardizeName("Name\ns.r o "));
        assertEquals("name|SR|", DigestUtils.standardizeName("Name spol. s.r.o. "));
        assertEquals("name|AS|", DigestUtils.standardizeName(" Name  akc.s."));
        assertEquals("|SR|name", DigestUtils.standardizeName("s.r.o. name"));
        assertEquals("ptacek", DigestUtils.standardizeName("Ptáček"));            
        assertEquals("name|GM|", DigestUtils.standardizeName("Name g m b h"));
    }
    
    /**
     * Tests of DigestUtils#standardizeAddress(java.langString) function.
     */
    @Test
    public void standardizeAddressTest() {
        //standardize address = street + city + country
        assertEquals("wall street new york city us", DigestUtils.standardizeAddress(new Address()
            .setStreet("Wall Street")
            .setCity("New York City")
            .setCountry("US")));
        //standardize address = street + city
        assertEquals("wall street new york city", DigestUtils.standardizeAddress(new Address()
            .setStreet("Wall Street")
            .setCity("New York City")));
        
        //standardize address = raw address + country
        assertEquals("ny, wall street us", DigestUtils.standardizeAddress(new Address()
            .setRawAddress("NY, Wall Street")
            .setCity("New York City")
            .setCountry("US")));
        //standardize address = raw address
        assertEquals("ny, wall street", DigestUtils.standardizeAddress(new Address()
            .setRawAddress("NY, Wall Street")
            .setCity("New York City")));
        
        assertEquals("ceske budejovice, lanova trida 1", DigestUtils.standardizeAddress(new Address()
            .setRawAddress("České Budějovice, Lanova třída 1")
            .setCity("České Budějovice")));
    }
    
    /**
     * Tests of DigestUtils#bodyHash(eu.dl.dataaccess.dto.matched.MatchedBody) function in case that the passed
     * MatchedBody has defined name and not empty list of body identifiers.
     */
    @Test
    public void bodyWithNameAndBodyIdsHashTest() {
        MatchedBody body = new MatchedBody()
            .setName("ABC")
            .addBodyId(new BodyIdentifier()
                .setId("222\t")
                .setScope(BodyIdentifier.Scope.CZ)
                .setType(BodyIdentifier.Type.VAT))
            .addBodyId(new BodyIdentifier()
                .setId("  111\n")
                .setScope(BodyIdentifier.Scope.CZ)
                .setType(BodyIdentifier.Type.ORGANIZATION_ID))            
            .addBodyId(new BodyIdentifier()
                .setId("222")
                .setScope(BodyIdentifier.Scope.CZ)
                .setType(BodyIdentifier.Type.HEADER_ICO))
            .addBodyId(new BodyIdentifier()
                .setId("333")
                .setScope(BodyIdentifier.Scope.CZ)
                .setType(BodyIdentifier.Type.ETALON_ID));
        
        String expectedHash =
            org.apache.commons.codec.digest.DigestUtils.sha256Hex("nameabcCZ111CZ222");
        
        assertEquals(expectedHash, DigestUtils.bodyHash(body));
    }
    
    /**
     * Tests of DigestUtils#digestName(java.lang.String) function.
     */
    @Test
    public void digestNameTest() {        
        assertEquals("a1b", DigestUtils.digestName(" \nA\t1 b"));
        assertEquals("1ac", DigestUtils.digestName("1---a-c"));
        assertEquals("1ab", DigestUtils.digestName("1 abc"));
        assertEquals(null, DigestUtils.digestName("ab"));
        assertEquals(null, DigestUtils.digestName("a b"));
    }
    
    /**
     * Tests of DigestUtils#digestAddress(java.lang.String) function.
     */
    @Test
    public void digestAddressTest() {        
        assertEquals("st123", DigestUtils.digestAddress("street 123"));
        assertEquals("st123", DigestUtils.digestAddress("s 123 treet"));
        assertEquals("st123", DigestUtils.digestAddress("st. street 123"));
        assertEquals("stst", DigestUtils.digestAddress("st. street"));
    }
    
    /**
     * Tests of DigestUtils#digestString(java.lang.String,java.lang.String,int) function.
     */
    @Test
    public void digestStringTest() {
        assertEquals("abc", DigestUtils.digestString("abc 123", "[a-z]", 3));
        assertEquals("ab1", DigestUtils.digestString("ab 1 c", "[a-z0-9]", 3));
        
        assertEquals("", DigestUtils.digestString("abc 123", "[a-z0-9]", 0));
        assertEquals("", DigestUtils.digestString("abc 123", "[a-z0-9]", -1));
        assertEquals("", DigestUtils.digestString("abc 123", "", 2));
        assertEquals("", DigestUtils.digestString("abc 123", null, 2));
        assertEquals("", DigestUtils.digestString("", "[a-z0-9]", 2));
        assertEquals("", DigestUtils.digestString(null, "[a-z0-9]", 2));
    }
    
    /**
     * Tests of DigestUtils#removeAccents(java.lang.String) function.
     */
    @Test
    public void removeAccentsTest() {
        assertEquals("abc", DigestUtils.removeAccents("ábč"));
        assertEquals("", DigestUtils.removeAccents(""));
    }
    
    /**
     * Tests of DigestUtils#digest(java.lang.String,java.lang.String) function
     *  and DigestUtils#digest(eu.dl.dataaccess.dto.matched.MatchedBody).
     */
    @Test
    public void digestTest() {
        assertEquals("nam" + DigestUtils.SEPARATOR + "st12", DigestUtils.digest("name", "street 12"));
        assertEquals("nam" + DigestUtils.SEPARATOR, DigestUtils.digest("nam e", null));
        assertEquals(DigestUtils.SEPARATOR + "st12", DigestUtils.digest(null, "street 12"));


        // defined city and street
        assertEquals("nam|ci12345", DigestUtils.digest(new MatchedBody()
            .setName("name")
            .setAddress(new Address()
                .setRawAddress("address 09876")
                .setCity("city")
                .setStreet("12345"))));


        // defined rawAddress
        assertEquals("nam|ad09876", DigestUtils.digest(new MatchedBody()
            .setName("name")
            .setAddress(new Address()
                .setRawAddress("address 09876")
                .setCity("city")
                .setPostcode("12345"))));


        // compose raw address
        assertEquals("nam|ci12345", DigestUtils.digest(new MatchedBody()
            .setName("name")
            .setAddress(new Address()
                .setCity("city")
                .setPostcode("12345"))));


        // lack of information for raw address composing
        assertEquals("nam|", DigestUtils.digest(new MatchedBody()
            .setName("name")
            .setAddress(new Address()
                .setCity("city"))));
        
        assertEquals("nam|", DigestUtils.digest(new MatchedBody().setName("name")));

    }

    /**
     * Test of {@link DigestUtils#replace(java.lang.String, java.lang.String, java.lang.String)} and
     * {@link DigestUtils#replace(java.lang.String, java.util.Map) } method.
     */
    @Test
    public void replaceTest() {
        String regex = "(s|spol s) r o";
        String repl = "S";

        assertEquals("Datlab|S|and friends", DigestUtils.replace("Datlab s. r. o. and friends", regex, repl));

        assertEquals("Datlab|S|", DigestUtils.replace("Datlab, SPOL. sro", regex, repl));
        assertEquals("Datlab, SPOL. sroasdf", DigestUtils.replace("Datlab, SPOL. sroasdf", regex, repl));

        assertEquals("kovosrot", DigestUtils.replace("kovosrot", regex, repl));

        assertEquals("Datlab|SR|", DigestUtils.replace("Datlab, SPOL. sro",
            DigestUtils.COMPANY_TYPE_REGEX_REPLACEMENT.get("CZ")));


        // more replacements
        Map<String, String> regexMap = new HashMap<>();
        regexMap.put("SR", "(s|spol s) r o");
        regexMap.put("AS", "as");        
        assertEquals("Datlab|SR||AS|", DigestUtils.replace("Datlab, SPOL. sro as", regexMap));
        assertEquals("|AS|Datlab|SR|", DigestUtils.replace("as Datlab, SPOL. sro", regexMap));

        
        // first longer
        regexMap = new HashMap<>();
        regexMap.put("MB", "m b h");
        regexMap.put("GM", "g m b h");
        regexMap.put("CO", "company, company xy");
        assertEquals("Abc|GM|", DigestUtils.replace("Abc g m b h", regexMap));
        assertEquals("Abc|CO|", DigestUtils.replace("Abc Company xy", regexMap));


        //nulls
        assertEquals("abc", DigestUtils.replace("abc", null));
        assertEquals(null, DigestUtils.replace(null, regexMap));
        assertEquals(null, DigestUtils.replace(null, "abc", "A"));
        assertEquals("abc", DigestUtils.replace("abc", null, "A"));
        assertEquals("abc", DigestUtils.replace("abc", "abc", null));
    }

    /**
     * Test {@link DigestUtils#bodyFullHash(eu.dl.dataaccess.dto.matched.MatchedBody)}.
     */
    @Test
    public void bodyComplexHashTest() {
        MatchedBody body1 = new MatchedBody()
            .setName("ABC")
            .setAddress(new Address()
                .setCity("city")
                .setStreet("street")
                .setNuts(Arrays.asList("NUTS1", "NUTS2"))
            )
            .addBodyId(new BodyIdentifier()
                .setId("111")
                .setScope(BodyIdentifier.Scope.CZ)
                .setType(BodyIdentifier.Type.ORGANIZATION_ID))
            .addBodyId(new BodyIdentifier()
                .setId("222")
                .setScope(BodyIdentifier.Scope.CZ)
                .setType(BodyIdentifier.Type.VAT))
            .setBuyerType(BuyerType.NATIONAL_AGENCY);

        MatchedBody body2 = new MatchedBody()
            .setName("ABC")
            .setAddress(new Address()
                .setCity("city")
                .setStreet("street")
                .setNuts(Arrays.asList("NUTS2", "NUTS1")))
            .addBodyId(new BodyIdentifier()
                .setId("222")
                .setScope(BodyIdentifier.Scope.CZ)
                .setType(BodyIdentifier.Type.HEADER_ICO))
            .addBodyId(new BodyIdentifier()
                .setId("111")
                .setScope(BodyIdentifier.Scope.SK)
                .setType(BodyIdentifier.Type.ORGANIZATION_ID))
            .setBuyerType(BuyerType.NATIONAL_AGENCY);
        
        assertEquals(DigestUtils.bodyFullHash(body1), DigestUtils.bodyFullHash(body2));
    }
}
