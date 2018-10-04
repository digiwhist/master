package eu.datlab.worker.matched;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.matched.plugin.MatchingResult;
import java.time.LocalDate;
import java.time.Month;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Test class of the TenderPublicationSourceIdsAndPublicationDatesMatchingPlugin.
 *
 * @author Tomas Mrazek
 */
public final class TenderPublicationSourceIdsAndPublicationDatesMatchingPluginTest {
    private final List<Publication> publications = Arrays.asList(
        new Publication().setSourceId("1")
            .setPublicationDate(LocalDate.of(2017, Month.JANUARY, 1)),
        new Publication().setSourceId("2")
            .setPublicationDate(LocalDate.of(2017, Month.JANUARY, 2)),
        new Publication().setSourceId("1")
            .setPublicationDate(LocalDate.of(2017, Month.FEBRUARY, 1)));

    /**
     * Test of cases when plugin finds match.
     */
    @Test
    public void matchedTest() {
        List<Publication> testPublications = Arrays.asList(publications.get(0).setIsIncluded(Boolean.TRUE),
            publications.get(1));

        MatchedTender test = new MatchedTender().setPublications(testPublications);

        //mocking of the MatchedTenderDAO
        MatchedTenderDAO mockedMatchedTenderDAO = mock(MatchedTenderDAO.class);

        Map<String, LocalDate> idsDatesMap = new HashMap<>();
        idsDatesMap.put(testPublications.get(0).getSourceId(), testPublications.get(0).getPublicationDate());
        idsDatesMap.put(testPublications.get(1).getSourceId(), testPublications.get(1).getPublicationDate());

        when(mockedMatchedTenderDAO.getByPublicationSourceIdsAndPublicationDates(idsDatesMap))
            .thenReturn(Arrays.asList(
                new MatchedTender()
                    .setGroupId("group 1").setPublications(Arrays.asList(publications.get(0))),
                new MatchedTender()
                    .setGroupId("group 2").setPublications(Arrays.asList(publications.get(1)))
            ));

        //plugin test
        BaseTenderMatchingPlugin plugin =
            new TenderPublicationSourceIdsAndPublicationDatesMatchingPlugin(mockedMatchedTenderDAO, false);

        MatchingResult result = plugin.match(test);

        assertEquals("group 1", result.getGroupId());
        assertEquals(result.getMatched(), true);
        assertEquals(result.getMatchedBy(), "tenderPublicationSourceIdsAndPublicationDate");
    }

    /**
     * Test of cases when plugin doesn't match any tender.
     */
    @Test
    public void umatchedTest() {
        MatchedTenderDAO mockedMatchedTenderDAO = mock(MatchedTenderDAO.class);
        BaseTenderMatchingPlugin plugin;
        MatchingResult result;

        
        when(mockedMatchedTenderDAO.getByPublicationSourceIdsAndPublicationDates(null))
            .thenReturn(Collections.emptyList());
        plugin = new TenderPublicationSourceIdsAndPublicationDatesMatchingPlugin(mockedMatchedTenderDAO, false);
        result = plugin.match(null);
        assertEquals(result.getMatched(), false);


        when(mockedMatchedTenderDAO.getByPublicationSourceIdsAndPublicationDates(Collections.emptyMap()))
            .thenReturn(Collections.emptyList());
        plugin = new TenderPublicationSourceIdsAndPublicationDatesMatchingPlugin(mockedMatchedTenderDAO, false);
        result = plugin.match(new MatchedTender());
        assertEquals(result.getMatched(), false);


        when(mockedMatchedTenderDAO.getByPublicationSourceIdsAndPublicationDates(Collections.emptyMap()))
            .thenReturn(Collections.emptyList());
        plugin = new TenderPublicationSourceIdsAndPublicationDatesMatchingPlugin(mockedMatchedTenderDAO, false);
        result = plugin.match(new MatchedTender()
            .setPublications(Arrays.asList(publications.get(2).setIsIncluded(Boolean.TRUE))));
        assertEquals(result.getMatched(), false);
    }
}
