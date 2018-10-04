package eu.datlab.worker.system;

import org.junit.Assert;
import org.junit.Test;

import eu.datlab.dataaccess.dto.matched.BVDEtalonBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.utils.DigestUtils;

/**
 * @author Tomas Mrazek
 */
public class BVDEtalonTest {

    /**
     * Test of etalon body degesting.
     */
    @Test
    public final void bvdEtalonBodyDigestingTest() {
       BVDEtalonBody etalon = new BVDEtalonBody()
           .setId("138")
           .setStandardizedName("g's")
           .setName("G'S")
           .setCity("ELY")
           .setStreet("BARWAY ROAD")
           .setPostcode("CB7 5TZ")
           .setNuts3("UK12")
           .setCountry("United Kingdom")
           .setCountryIsoCode("GB")
           .setCompanyCategory("Very large company")
           .setStatus("Active")
           .setNumberOfEmployees("2000")
           .setNace("Growing of other non-perennial crops");

       MatchedBody matched = etalon.getAsMatchedBody();

       etalon.setStandardizedName(DigestUtils.standardizeName(matched.getName()));
       etalon.setStandardizedAddress(DigestUtils.standardizeAddress(matched.getAddress()));
       etalon.setDigest(DigestUtils.digest(matched));

       Assert.assertEquals("g's", etalon.getStandardizedName());
       Assert.assertEquals("barway road ely gb", etalon.getStandardizedAddress());
       Assert.assertEquals("|barw", etalon.getDigest());

       // digest2 accepts name digest of length two characters instead of three like DigestUtils.digest
       Assert.assertEquals("gs|barw", BVDEtalonDigestsWorker.digest2(matched));
    }

    /**
    * Test of etalon body degesting.
    */
    @Test
    public final void bvdEtalonBodyAsMatchedBodyTest() {
        // nuts cleanin
        Assert.assertEquals("UK12",
            new BVDEtalonBody().setNuts3("UK12").getAsMatchedBody().getAddress().getNuts().get(0));
        Assert.assertEquals("UK12",
            new BVDEtalonBody().setNuts3("lorem impsum,UK12").getAsMatchedBody().getAddress().getNuts().get(0));
        Assert.assertEquals("UK12",
            new BVDEtalonBody().setNuts3("UK12 lorem impsum").getAsMatchedBody().getAddress().getNuts().get(0));
        Assert.assertEquals("UK12C",
            new BVDEtalonBody().setNuts3("UK12C").getAsMatchedBody().getAddress().getNuts().get(0));

        Assert.assertNull(
            new BVDEtalonBody().setNuts3("UK12lorem impsum").getAsMatchedBody().getAddress());
    }
}
