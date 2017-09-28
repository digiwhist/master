package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.master.plugin.generic.ModusPlugin;
import eu.dl.worker.master.plugin.generic.converter.TenderConverter;
import org.junit.BeforeClass;
import org.junit.Test;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;

import static org.junit.Assert.assertTrue;

/**
 * Modus plugin tests.
 */
public class ModusPluginTest {

    private static final String stringValueName = "appealBodyName";
    private static final String dateValueName = "bidDeadline";
    private static final String bodyValueName = "furtherInformationProvider";
    
    private static final String shortString = "a";
    private static final String longString = "aaaaaa";

    private static MatchedTender matchedTender1;

    private static MatchedTender matchedTender2;

    private static MatchedBody matchedBody1;
    
    private static MatchedBody matchedBody2;
    
    private MasterTender masterTender = new MasterTender();

    /**
     * Prepares data for the testing purposes.
     */
    @BeforeClass
    public static void prepareData() {
        matchedTender1 = new MatchedTender();
        matchedTender1.setId("matchedTender1");
        matchedTender1.setAppealBodyName(shortString);
        matchedTender1.setBidDeadline(LocalDateTime.MIN);
        
        Publication publication1 = new Publication();
        publication1.setIsIncluded(true);
        publication1.setPublicationDate(LocalDate.MIN);
        
        matchedTender1.setPublications(Arrays.asList(publication1));
        
        matchedBody1 = new MatchedBody();
        matchedBody1.setId("matchedBody1");
        matchedBody1.setGroupId("matchedBody1");
        matchedTender1.setFurtherInformationProvider(matchedBody1);
        
        matchedTender2 = new MatchedTender();
        matchedTender2.setId("matchedTender2");
        matchedTender2.setAppealBodyName(longString);
        matchedTender2.setBidDeadline(LocalDateTime.MAX);
        
        Publication publication2 = new Publication();
        publication2.setIsIncluded(true);
        publication2.setPublicationDate(LocalDate.MAX);
        
        matchedTender2.setPublications(Arrays.asList(publication2));
        
        matchedBody2 = new MatchedBody();
        matchedBody2.setId("matchedBody2");
        matchedBody2.setGroupId("matchedBody2");
        matchedTender2.setFurtherInformationProvider(matchedBody2);
    }
    
    /**
     * Test ModusPlugin for tender fields.
     */
    @Test
    public final void modusTestTenderStringFields() {        
        MasterPlugin masterPlugin = new ModusPlugin(stringValueName, new TenderConverter());

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2, matchedTender2, matchedTender2,
                matchedTender1),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender2, matchedTender2, matchedTender2,
                        matchedTender1));
        assertTrue(masterTender.getAppealBodyName().equals(longString));

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender1, matchedTender2, matchedTender1,
                matchedTender2),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender1, matchedTender2, matchedTender1,
                        matchedTender2));
        assertTrue(masterTender.getAppealBodyName().equals(shortString));

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2, matchedTender1, matchedTender2),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender2, matchedTender1, matchedTender2));
        assertTrue(masterTender.getAppealBodyName().equals(longString));
    }
    
    /**
     * Test ModusPlugin for tender fields.
     */
    @Test
    public final void modusTestTenderDateFields() {        
        MasterPlugin masterPlugin = new ModusPlugin(dateValueName, new TenderConverter());

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2, matchedTender2, matchedTender2,
                matchedTender1),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender2, matchedTender2, matchedTender2,
                        matchedTender1));
        assertTrue(masterTender.getBidDeadline().compareTo(LocalDateTime.MAX) == 0);

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender1, matchedTender2, matchedTender1,
                matchedTender2),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender1, matchedTender2, matchedTender1,
                        matchedTender2));
        assertTrue(masterTender.getBidDeadline().compareTo(LocalDateTime.MIN) == 0);

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2, matchedTender1, matchedTender2),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender2, matchedTender1, matchedTender2));
        assertTrue(masterTender.getBidDeadline().compareTo(LocalDateTime.MAX) == 0);
    }
    
    /**
     * Test ModusPlugin is sorting array correctly.
     */
    @Test
    public final void modusTestBody() {      
        MasterPlugin masterPlugin = new ModusPlugin(bodyValueName, new TenderConverter());

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2, matchedTender2, matchedTender2,
                matchedTender1),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender2, matchedTender2, matchedTender2,
                        matchedTender1));
        assertTrue(masterTender.getFurtherInformationProvider().getGroupId().compareTo(matchedBody2.getId()) == 0);

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender1, matchedTender2, matchedTender1,
                matchedTender2),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender1, matchedTender2, matchedTender1,
                        matchedTender2));
        assertTrue(masterTender.getFurtherInformationProvider().getGroupId().compareTo(matchedBody1.getId()) == 0);

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2, matchedTender1, matchedTender2), 
                masterTender,
                Arrays.asList(matchedTender1, matchedTender2, matchedTender1, matchedTender2));
        assertTrue(masterTender.getFurtherInformationProvider().getGroupId().compareTo(matchedBody2.getId()) == 0);
    }
}
