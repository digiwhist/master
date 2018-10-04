package eu.dl.worker.master.utils;

import static org.junit.Assert.assertNull;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Currency;

import org.junit.Before;
import org.junit.Test;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

/**
 * Test of tender size utils.
 *
 */
public final class TenderSizeUtilsTest {

    private final MasterTender tender1 = new MasterTender()
            .setCpvs(Arrays.asList(
                    new CPV().setCode("75200")
            ))
            .setProcedureType(TenderProcedureType.DESIGN_CONTEST)
            .addBuyer(new MasterBody().addMainActivity(BuyerActivityType.DEFENCE))
            .addLot(new MasterTenderLot())
            .addLot(new MasterTenderLot())
            .addLot(new MasterTenderLot())
            .addLot(new MasterTenderLot())
            .addLot(new MasterTenderLot())
            .addLot(new MasterTenderLot());

    private final MasterTender tender2 = new MasterTender()
            .setCpvs(Arrays.asList(
                    new CPV().setCode("71200")
            ))
            .setProcedureType(TenderProcedureType.NEGOTIATED)
            .addBuyer(new MasterBody().addMainActivity(BuyerActivityType.COAL_AND_OTHER_EXTRACTION))
            .setPublications(Arrays.asList(
                    new Publication()
                            .setPublicationDate(LocalDate.of(2007, 12, 23))
                            .setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setIsIncluded(true)))
            .addLot(new MasterTenderLot()
                        .setRobustEstimatedPrice(
                                new Price()
                                        .setCurrencyNational(Currency.getInstance("CZK"))
                                        .setNetAmountNational(new BigDecimal(1000000))
            ));

    /**
     * Init.
     */
    @Before
    public void init() {
        Config.getInstance().addConfigFile("unit_test");
    }

    /**
     * Test of cases where the tender size shouldn't be calculated.
     */
    @Test
    public void doNotCalculateTest() {
        assertNull(TenderSizeUtils.calculate(tender1));
        tender1.setCpvs(null);
        assertNull(TenderSizeUtils.calculate(tender1));
        tender1.setProcedureType(null);
        assertNull(TenderSizeUtils.calculate(tender1));
        tender1.setBuyers(null);
        assertNull(TenderSizeUtils.calculate(tender1));
    }

    /**
     * Test of cases where the tender size should be calculated.
     */
    @Test
    public void calculateTest() {
        //assertNull(TenderSizeUtils.calculate(tender2));
    }
}
