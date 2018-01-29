package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dao.CleanTenderDAO;
import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.generic.AwardCriterion;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test of key missing fields indicator plugin.
 *
 * @author Tomas Mrazek
 */
public final class KeyMissingFieldsIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender()
        .setPublications(Arrays.asList(new Publication()
                .setIsIncluded(true).setFormType(PublicationFormType.CONTRACT_AWARD)));

    private final MasterTender masterTender = new MasterTender().setGroupId("group-1");

    private final List<CleanTender> cleanTenders = Arrays.asList(
        new CleanTender()
            .setSelectionMethod(SelectionMethod.MEAT)
            .setAwardCriteria(Arrays.asList(new AwardCriterion())),
        new CleanTender()
            .setAddressOfImplementation(new Address().addNuts("CZ001"))            
    );

    
    private final KeyMissingFieldsIndicatorPlugin plugin;

    /**
     * Constructor with DAO class mocking and plugin instatiation.
     */
    public KeyMissingFieldsIndicatorPluginTest() {        
        // matched tenders init
        List<MatchedTender> matchedTenders = Arrays.asList(new MatchedTender(), new MatchedTender());
        matchedTenders.get(0)
            .setPublications(Arrays.asList(new Publication()
                .setIsIncluded(true).setFormType(PublicationFormType.CONTRACT_NOTICE)))
            .setCleanObjectId("clean-1");
        matchedTenders.get(1)
            .setPublications(Arrays.asList(new Publication()
                .setIsIncluded(true).setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setCleanObjectId("clean-2");

        //mocking of the MatchedTenderDAO
        MatchedTenderDAO mockedMatchedTenderDAO = mock(MatchedTenderDAO.class);
        when(mockedMatchedTenderDAO.getByGroupId("group-1")).thenReturn(matchedTenders);

        //mocking of the CleanTenderDAO
        CleanTenderDAO mockedCleanTenderDAO = mock(CleanTenderDAO.class);
        when(mockedCleanTenderDAO.getById("clean-1")).thenReturn(cleanTenders.get(0));
        when(mockedCleanTenderDAO.getById("clean-2")).thenReturn(cleanTenders.get(1));

        plugin = new KeyMissingFieldsIndicatorPlugin(mockedMatchedTenderDAO, mockedCleanTenderDAO);
    }



    /**
     * Test of correct tender address.
     */
    @Test
    public void insufficientDataIndicatorTest() {
        Indicator indicator = plugin.evaluate(nullTender);
        assertEquals(TenderIndicatorType.TRANSPARENCY_NUMBER_OF_KEY_MISSING_FIELDS.name(), indicator.getType());
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA, indicator.getStatus());
        assertNull(indicator.getValue());

        indicator = plugin.evaluate(null);
        assertEquals(TenderIndicatorType.TRANSPARENCY_NUMBER_OF_KEY_MISSING_FIELDS.name(), indicator.getType());
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA, indicator.getStatus());
        assertNull(indicator.getValue());
    }

    /**
     * Test of positive result.
     */
    @Test
    public void calculatedTest() {
        Indicator indicator = plugin.evaluate(masterTender);
        assertEquals(TenderIndicatorType.TRANSPARENCY_NUMBER_OF_KEY_MISSING_FIELDS.name(), indicator.getType());
        assertEquals(IndicatorStatus.CALCULATED, indicator.getStatus());
        assertEquals(new Double(40), indicator.getValue());
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void getTypeTest() {
        assertEquals(plugin.getType(), TenderIndicatorType.TRANSPARENCY_NUMBER_OF_KEY_MISSING_FIELDS.name());
    }
}
