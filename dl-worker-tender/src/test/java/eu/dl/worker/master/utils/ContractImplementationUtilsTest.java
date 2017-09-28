package eu.dl.worker.master.utils;

import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.generic.Payment;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.dataaccess.dto.matched.StructuredLotId;
import org.junit.Test;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Currency;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNull;

/**
 * Test of contract implemenations.
 *
 */
public final class ContractImplementationUtilsTest {
    private static final String TENDER1_ID = "tender1ID";
    private static final String TENDER2_ID = "tender2ID";
    private static final String TENDER3_ID = "tender3ID";

    private final MatchedTender tender1 = new MatchedTender()
            .setAddressOfImplementation(new Address()
                    .addNuts("NutsCode1"))
            .setDocumentsLocation(new Address()
                    .addNuts("NutsCode2"))
            .addLot(new MatchedTenderLot()
                    .setAddressOfImplementation(new Address()
                            .setCity("SomeCity"))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER1_ID)))
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true)
                    .setPublicationDate(LocalDate.now().minusDays(2))));
    private final MatchedTender tender2 = new MatchedTender()
            .setAddressOfImplementation(new Address()
                    .setCity("SomeCity"))
            .setDocumentsLocation(new Address()
                    .addNuts("NutsCode2")
                    .setCity("SomeCity"))
            .addLot(new MatchedTenderLot()
                    .setAddressOfImplementation(new Address()
                            .setCity("SomeCity"))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER2_ID)))
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true)
                    .setPublicationDate(LocalDate.now().minusDays(1))));
    private final MatchedTender tender3 = new MatchedTender()
            .setAddressOfImplementation(new Address()
                    .setCity("SomeCity")
                    .setPostcode("SomePostalCode"))
            .setDocumentsLocation(new Address()
                    .addNuts("NutsCode2"))
            .addLot(new MatchedTenderLot()
                    .setAddressOfImplementation(new Address()
                            .setCity("SomeCity"))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER3_ID)))
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true)
                    .setPublicationDate(LocalDate.now())));


    private final Payment paymentA = new Payment()
            .setPaymentDate(LocalDate.MIN)
            .setPrice(new Price()
                    .setNetAmount(new BigDecimal(1000))
                    .setCurrency(Currency.getInstance("CZK")));

    private final Payment paymentB = new Payment()
            .setPaymentDate(LocalDate.MIN)
            .setPrice(new Price()
                    .setNetAmount(new BigDecimal(1000))
                    .setCurrency(Currency.getInstance("CZK")));

    private final Payment paymentC = new Payment()
            .setPaymentDate(LocalDate.MIN)
            .setPrice(new Price()
                    .setNetAmount(new BigDecimal(100))
                    .setCurrency(Currency.getInstance("CZK")));

    private final Payment paymentD = new Payment()
            .setPaymentDate(LocalDate.MIN)
            .setPrice(new Price()
                    .setNetAmount(new BigDecimal(100))
                    .setCurrency(Currency.getInstance("EUR")));

    /**
     * Default constructor to set tender IDs. The setter does not support fluent interface, so we call it here.
     */
    public ContractImplementationUtilsTest() {
        tender1.setId(TENDER1_ID);
        tender2.setId(TENDER2_ID);
        tender3.setId(TENDER3_ID);
    }

    /**
     * Test of correct payment comparsion.
     */
    @Test
    public void comparePaymentTest() {
        assertFalse(ContractImplementationUtils.compare((Payment) null, null));
        assertFalse(ContractImplementationUtils.compare(paymentA, null));
        assertFalse(ContractImplementationUtils.compare(null, paymentB));

        assertTrue(ContractImplementationUtils.compare(paymentA, paymentB));
        assertFalse(ContractImplementationUtils.compare(paymentA, paymentC));
        assertFalse(ContractImplementationUtils.compare(paymentA, paymentC));
        assertFalse(ContractImplementationUtils.compare(paymentA.setPaymentDate(null), paymentB));
        assertFalse(ContractImplementationUtils.compare(paymentA, paymentB.setPaymentDate(null)));
        assertFalse(ContractImplementationUtils.compare(paymentC, paymentD));

        paymentD.getPrice().setCurrency(Currency.getInstance("CZK"));
        assertTrue(ContractImplementationUtils.compare(paymentC, paymentD));

        paymentD.setPrice(null);
        assertFalse(ContractImplementationUtils.compare(paymentC, paymentD));
    }

    /**
     * Test of correct payment merge.
     */
    @Test
    public void addPaymentTest() {
        assertNull(ContractImplementationUtils.addPayments(
                null, null));

        List<Payment> listA = new ArrayList<>();
        listA.addAll(Arrays.asList(paymentA, paymentB));

        List<Payment> listB = new ArrayList<>();
        listB.addAll(Arrays.asList(paymentA, paymentC));

        List<Payment> listC = ContractImplementationUtils.addPayments(listA, listB);
        assertEquals(3, listC.size());

        listA.add(paymentD);
        List<Payment> listD = ContractImplementationUtils.addPayments(listA, listB);
        assertEquals(4, listD.size());
    }
}
