package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import org.junit.Test;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import static eu.dl.dataaccess.dto.codetables.BodyIdentifier.Type.BVD_ID;
import static org.junit.Assert.assertEquals;

/**
 * Political connections test.
 */
public class PoliticalConnectionsOfSuppliersTest {

    private final MasterTender undefined = new MasterTender()
            .setPublications(Arrays.asList(new Publication()));

    private final MasterTender insufficient1 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setFormType(PublicationFormType.CONTRACT_AWARD)));

    private final MasterTender insufficient2 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender insufficient3 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(new MasterBid()))));

    private final MasterTender insufficient4 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(new MasterBid()
                            .setBidders(Arrays.asList(new MasterBody()))))));

    private final MasterTender insufficient5 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(new MasterBid()
                            .setBidders(Arrays.asList(new MasterBody()
                                    .setBodyIds(Arrays.asList(new BodyIdentifier()))))))));

    private final MasterTender insufficient6 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(new MasterBid()
                            .setBidders(Arrays.asList(new MasterBody()
                                    .setBodyIds(Arrays.asList(new BodyIdentifier()
                                            .setId("lorem")))))))));

    private final MasterTender insufficient7 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(new MasterBid()
                            .setBidders(Arrays.asList(new MasterBody()
                                    .setBodyIds(Arrays.asList(new BodyIdentifier()
                                            .setType(BVD_ID)))))))));

    private final MasterTender insufficient8 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(new MasterBid()
                            .setBidders(Arrays.asList(new MasterBody()
                                    .setBodyIds(Arrays.asList(new BodyIdentifier()
                                            .setId("lorem")
                                            .setType(BVD_ID)))))))));

    private final MasterTender allMatch1 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(new MasterBid()
                            .setIsWinning(true)
                            .setBidders(Arrays.asList(new MasterBody()
                                    .setBodyIds(Arrays.asList(new BodyIdentifier()
                                            .setId("lorem")
                                            .setType(BVD_ID)))))))));

    private final MasterTender allMatch2 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(new MasterBid()
                            .setIsWinning(true)
                            .setBidders(Arrays.asList(new MasterBody()
                                    .setBodyIds(Arrays.asList(
                                            new BodyIdentifier()
                                                    .setId("lorem")
                                                    .setType(BVD_ID),
                                            new BodyIdentifier()
                                                    .setId("lorem")
                                                    .setType(BVD_ID)))))))));

    private final MasterTender anyMatch1 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(new MasterBid()
                            .setIsWinning(true)
                            .setBidders(Arrays.asList(new MasterBody()
                                    .setBodyIds(Arrays.asList(
                                            new BodyIdentifier()
                                                    .setType(BVD_ID),
                                            new BodyIdentifier()
                                                    .setId("lorem")
                                                    .setType(BVD_ID)))))))));


    private final PoliticalConnectionsOfSuppliers pluginTrue = new PoliticalConnectionsOfSuppliers(
            new MasterBodyDAOtest() {
                @Override
                public boolean existsInPoliticalExposedPersons(final String bvdIdNumber) {
                    return true;
                }
            });

    private final PoliticalConnectionsOfSuppliers pluginFalse = new PoliticalConnectionsOfSuppliers(
            new MasterBodyDAOtest() {
                @Override
                public boolean existsInPoliticalExposedPersons(final String bvdIdNumber) {
                    return false;
                }
            });

    /**
     * Test of undefined result.
     */
    @Test
    public final void undefinedTest() {
        assertEquals(pluginTrue.evaluate(undefined).getStatus(), IndicatorStatus.UNDEFINED);
        assertEquals(pluginFalse.evaluate(undefined).getStatus(), IndicatorStatus.UNDEFINED);
    }

    /**
     * Test of insufficient result.
     */
    @Test
    public final void insufficientTest() {
        assertEquals(pluginTrue.evaluate(insufficient1).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginTrue.evaluate(insufficient2).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginTrue.evaluate(insufficient3).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginTrue.evaluate(insufficient4).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginTrue.evaluate(insufficient5).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginTrue.evaluate(insufficient6).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginTrue.evaluate(insufficient7).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginTrue.evaluate(insufficient8).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginFalse.evaluate(insufficient1).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginFalse.evaluate(insufficient2).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginFalse.evaluate(insufficient3).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginFalse.evaluate(insufficient4).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginFalse.evaluate(insufficient5).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginFalse.evaluate(insufficient6).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginFalse.evaluate(insufficient7).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginFalse.evaluate(insufficient8).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginFalse.evaluate(allMatch1).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginFalse.evaluate(allMatch2).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(pluginFalse.evaluate(anyMatch1).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
    }

    /**
     * Test of positive result.
     */
    @Test
    public final void allMatchTest() {
        assertEquals(pluginTrue.evaluate(allMatch1).getValue(), Double.valueOf(0));
        assertEquals(pluginTrue.evaluate(allMatch1).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(pluginTrue.evaluate(allMatch2).getValue(), Double.valueOf(0));
        assertEquals(pluginTrue.evaluate(allMatch2).getStatus(), IndicatorStatus.CALCULATED);

        assertEquals(pluginTrue.evaluate(anyMatch1).getValue(), Double.valueOf(100));
        assertEquals(pluginTrue.evaluate(anyMatch1).getStatus(), IndicatorStatus.CALCULATED);
    }

    /**
     * Test of correct type.
     */
    @Test
    public final void getTypeTest() {
        assertEquals(pluginTrue.getType(),
                TenderIndicatorType.TRANSPARENCY_NUMBER_OF_KEY_MISSING_FIELDS.name());
        assertEquals(pluginFalse.getType(),
                TenderIndicatorType.TRANSPARENCY_NUMBER_OF_KEY_MISSING_FIELDS.name());
    }

    /**
     * MasterBodyDao for test.
     */
    private abstract class MasterBodyDAOtest implements MasterBodyDAO {

        @Override
        public String save(final MasterBody item) {
            return null;
        }

        @Override
        public MasterBody getById(final String id) {
            return null;
        }

        @Override
        public List getMine(final String name, final String version, final String fromDate, final String toDate) {
            return null;
        }

        @Override
        public List getByGroupId(final String groupId) {
            return null;
        }

        @Override
        public String save(final Object masteredItem) {
            return null;
        }

        @Override
        public List getByGroupIds(final Collection groupIds) {
            return null;
        }

        @Override
        public List getModifiedAfter(final LocalDateTime timestamp, final Integer page) {
            return null;
        }

        @Override
        public List getModifiedAfter(final LocalDateTime timestamp, final String modifiedBy, final Integer page) {
            return null;
        }

        @Override
        public List getModifiedAfter(final LocalDateTime timestamp, final String modifiedBy, final String country, final Integer page) {
            return null;
        }

        @Override
        public MasterBody getEmptyInstance() {
            return null;
        }
    }
}
