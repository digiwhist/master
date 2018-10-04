package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import java.time.LocalDate;
import java.time.Month;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import org.junit.Assert;


import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test of key missing fields indicator plugin.
 *
 * @author Tomas Mrazek
 */
public final class NoticeAndAwardDiscrepanciesIndicatorPluginTest {

    private final MasterTender masterTender = new MasterTender().setGroupId("group-1");

    private final MasterTender nullTender = new MasterTender().setGroupId("group-2");

    private final LocalDate newDate = LocalDate.of(2017, Month.JANUARY, 1);
    private final LocalDate oldDate = LocalDate.of(2000, Month.JANUARY, 1);

    private final MatchedTender newNotice =  new MatchedTender()
        .setPublications(Arrays.asList(new Publication()
            .setIsIncluded(true).setFormType(PublicationFormType.CONTRACT_NOTICE)
            .setPublicationDate(newDate)))
        .setTitle("notice title")
        .setIsDps(Boolean.FALSE);
    private final MatchedTender oldNotice = new MatchedTender()
        .setPublications(Arrays.asList(new Publication()
            .setIsIncluded(true).setFormType(PublicationFormType.CONTRACT_NOTICE)
            .setPublicationDate(oldDate)));
    private final MatchedTender newAward =  new MatchedTender()
        .setPublications(Arrays.asList(new Publication()
            .setIsIncluded(true).setFormType(PublicationFormType.CONTRACT_AWARD)
            .setPublicationDate(newDate)))
        .setTitle("award title")
        .setIsDps(Boolean.FALSE);
    private final MatchedTender oldAward =  new MatchedTender()
        .setPublications(Arrays.asList(new Publication()
            .setIsIncluded(true).setFormType(PublicationFormType.CONTRACT_AWARD)
            .setPublicationDate(oldDate)));

    
    private final NoticeAndAwardDiscrepanciesIndicatorPlugin plugin;

    /**
     * Constructor with DAO class mocking and plugin instatiation.
     */
    public NoticeAndAwardDiscrepanciesIndicatorPluginTest() {
        //mocking of the MatchedTenderDAO
        MatchedTenderDAO mockedMatchedTenderDAO = mock(MatchedTenderDAO.class);
        when(mockedMatchedTenderDAO.getByGroupId(masterTender.getGroupId())).thenReturn(Arrays.asList(newNotice,
            oldNotice, newAward, oldAward));
        when(mockedMatchedTenderDAO.getByGroupId(nullTender.getGroupId())).thenReturn(Arrays.asList(newAward));

        plugin = new NoticeAndAwardDiscrepanciesIndicatorPlugin(mockedMatchedTenderDAO);
    }



    /**
     * Test of states when the indicator can't be calculated.
     */
    @Test
    public void insufficientDataIndicatorTest() {
        Indicator indicator = plugin.evaluate(null);
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA, indicator.getStatus());
        assertNull(indicator.getValue());

        indicator = plugin.evaluate(new MasterTender());
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA, indicator.getStatus());
        assertNull(indicator.getValue());

        indicator = plugin.evaluate(new MasterTender().setPublications(Collections.emptyList()));
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA, indicator.getStatus());
        assertNull(indicator.getValue());
        
        indicator = plugin.evaluate(new MasterTender().setPublications(Collections.emptyList()));
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA, indicator.getStatus());
        assertNull(indicator.getValue());

        indicator = plugin.evaluate(nullTender);
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA, indicator.getStatus());
        assertNull(indicator.getValue());
    }

    /**
     * Test of positive result.
     */
    @Test
    public void calculatedTest() {
        Indicator indicator = plugin.evaluate(masterTender);
        assertEquals(IndicatorStatus.CALCULATED, indicator.getStatus());
        Assert.assertTrue(indicator.getValue() > 0);
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void getTypeTest() {
        assertEquals(plugin.getType(), TenderIndicatorType.ADMINISTRATIVE_NOTICE_AND_AWARD_DISCREPANCIES.name());
    }
}
