package eu.dl.worker.master.plugin.body;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier.Scope;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier.Type;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.matched.EtalonBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.worker.master.plugin.MasterPlugin;
import org.junit.Test;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertTrue;

/**
 * Test of bodyidmastering plugin.
 */
public class BodyIdPluginTest {
    private MatchedBody nullBody1 = new MatchedBody().setBodyIds(null);
    
    private MatchedBody nullBody2 = new MatchedBody().setBodyIds(Arrays.asList(
            new BodyIdentifier().setId("a").setScope(null).setType(Type.ORGANIZATION_ID),
            new BodyIdentifier().setId("b").setScope(Scope.AD).setType(null),
            new BodyIdentifier().setId("b").setScope(null).setType(null),
            new BodyIdentifier().setId(null).setScope(null).setType(null)))
            .setPublicationDate(LocalDate.MIN);
    
    private MatchedBody nullBody3 = new MatchedBody().setBodyIds(Arrays.asList(
            new BodyIdentifier().setId(null).setScope(null).setType(null)));
    
    private MatchedBody matchedBody1 = new MatchedBody().setBodyIds(Arrays.asList(
            new BodyIdentifier().setId("a").setScope(Scope.AD).setType(Type.ORGANIZATION_ID),
            new BodyIdentifier().setId("b").setScope(Scope.AD).setType(Type.HEADER_ICO)))
            .setPublicationDate(LocalDate.MIN);

    private MatchedBody matchedBody2 = new MatchedBody().setBodyIds(Arrays.asList(
            new BodyIdentifier().setId("a").setScope(Scope.AD).setType(Type.ORGANIZATION_ID),
            new BodyIdentifier().setId("c").setScope(Scope.AD).setType(Type.HEADER_ICO)))
            .setPublicationDate(LocalDate.now());
    
    private MatchedBody matchedBody3 = new MatchedBody().setBodyIds(Arrays.asList(
            new BodyIdentifier().setId("b").setScope(Scope.AD).setType(Type.ORGANIZATION_ID),
            new BodyIdentifier().setId("d").setScope(Scope.AD).setType(Type.HEADER_ICO)))
            .setPublicationDate(LocalDate.MIN);
    
    private MatchedBody matchedBody4 = new MatchedBody().setBodyIds(Arrays.asList(
            new BodyIdentifier().setId("a").setScope(Scope.AD).setType(Type.ORGANIZATION_ID),
            new BodyIdentifier().setId("et").setScope(Scope.AD).setType(Type.ETALON_ID)));
    
    private MatchedBody matchedBody5 = new MatchedBody().setSource(EtalonBody.ETALON_SOURCE_ID)
            .setBodyIds(Arrays.asList(
                    new BodyIdentifier().setId("etalon").setScope(Scope.AD).setType(Type.ETALON_ID)));

    /**
     * Null values test.
     */
    @Test
    public final void nullTest() {
        MasterPlugin masterPlugin = new BodyIdPlugin();
        
        // no ids at all
        MasterBody masterBody = new MasterBody().setMasterBy("test");
        masterPlugin.master(Arrays.asList(nullBody1), masterBody, Arrays.asList(nullBody1));
        assertTrue(masterBody.getBodyIds() == null);
        
        // empty source and type
        masterBody  = new MasterBody().setMasterBy("test");
        masterPlugin.master(Arrays.asList(nullBody1, nullBody2), masterBody, Arrays.asList(nullBody1, nullBody2));
        List<BodyIdentifier> ids = masterBody.getBodyIds();
        assertTrue(ids.size() == 3);
        assertContainsBodyId(ids, "a", null, Type.ORGANIZATION_ID);
        assertContainsBodyId(ids, "b", Scope.AD, null);
        assertContainsBodyId(ids, "b", null, null);
        
        // empty body ids
        masterBody = new MasterBody().setMasterBy("test");
        masterPlugin.master(Arrays.asList(nullBody3), masterBody, Arrays.asList(nullBody3));
        assertTrue(masterBody.getBodyIds() == null);
    }
    
    /**
     * Test.
     */
    @Test
    public final void basicTest() {
        MasterPlugin masterPlugin = new BodyIdPlugin();
        MasterBody masterBody = new MasterBody().setMasterBy("test");
                
        masterPlugin.master(Arrays.asList(matchedBody1), masterBody, Arrays.asList(matchedBody1));
        assertTrue(masterBody.getBodyIds().size() == 2);

        masterPlugin.master(Arrays.asList(matchedBody1, matchedBody2), masterBody,
                Arrays.asList(matchedBody1, matchedBody2));
        assertTrue(masterBody.getBodyIds().size() == 2);
    }
    
    /**
     * Test etalon preference.
     */
    @Test
    public final void etalonPreferrenceTest() {
        MasterPlugin masterPlugin = new BodyIdPlugin();
        
        MasterBody masterBody = new MasterBody().setMasterBy("test");
        masterPlugin.master(Arrays.asList(matchedBody4, matchedBody5), masterBody,
                Arrays.asList(matchedBody4, matchedBody5));
        
        List<BodyIdentifier> ids = masterBody.getBodyIds();
        assertTrue(ids.size() == 2);
        assertContainsBodyId(ids, "etalon", Scope.AD, Type.ETALON_ID);
        
        masterBody = new MasterBody().setMasterBy("test");
        masterPlugin.master(Arrays.asList(matchedBody5), masterBody,
                Arrays.asList(matchedBody5));
        ids = masterBody.getBodyIds();
        assertTrue(ids.size() == 1);
        assertContainsBodyId(ids, "etalon", Scope.AD, Type.ETALON_ID);
    }
    
    /**
     * Test most often values.
     */
    @Test
    public final void mostCommonValueTest() {
        MasterPlugin masterPlugin = new BodyIdPlugin();
        MatchedBody mB1 = new MatchedBody().setBodyIds(Arrays.asList(
                new BodyIdentifier().setId("c").setScope(Scope.AD).setType(Type.ORGANIZATION_ID)))
                .setPublicationDate(LocalDate.now());
        
        MatchedBody mB2 = new MatchedBody().setBodyIds(Arrays.asList(
                new BodyIdentifier().setId("a").setScope(Scope.AD).setType(Type.ORGANIZATION_ID)))
                .setPublicationDate(LocalDate.now());
        
        MatchedBody mB3 = new MatchedBody().setBodyIds(Arrays.asList(
                new BodyIdentifier().setId("a").setScope(Scope.AD).setType(Type.ORGANIZATION_ID)))
                .setPublicationDate(LocalDate.now());
        
        MatchedBody mB4 = new MatchedBody().setBodyIds(Arrays.asList(
                new BodyIdentifier().setId("c").setScope(Scope.AD).setType(Type.ORGANIZATION_ID)))
                .setPublicationDate(LocalDate.now());
        
        MatchedBody mB5 = new MatchedBody().setBodyIds(Arrays.asList(
                new BodyIdentifier().setId("a").setScope(Scope.AD).setType(Type.ORGANIZATION_ID)))
                .setPublicationDate(LocalDate.now());
        
        MasterBody masterBody = new MasterBody().setMasterBy("test");
        masterPlugin.master(Arrays.asList(mB1, mB2, mB3, mB4, mB5), masterBody,
                Arrays.asList(mB1, mB2, mB3, mB4, mB5));
        
        List<BodyIdentifier> ids = masterBody.getBodyIds();
        assertTrue(ids.size() == 1);
        assertContainsBodyId(ids, "a", Scope.AD, Type.ORGANIZATION_ID);  
    }
    
    /**
     * Test newest values.
     */
    @Test
    public final void mostCommonAndNewestValueTest() {
        MasterPlugin masterPlugin = new BodyIdPlugin();
        
        MasterBody masterBody = new MasterBody().setMasterBy("test");
        masterPlugin.master(Arrays.asList(matchedBody1, matchedBody2, matchedBody3), masterBody,
                Arrays.asList(matchedBody1, matchedBody2, matchedBody3));
        
        List<BodyIdentifier> ids = masterBody.getBodyIds();
        assertTrue(ids.size() == 2);
        assertContainsBodyId(ids, "c", Scope.AD, Type.HEADER_ICO);  
    }
    
    /**
     * Checks, whether the list contains body identifier.
     * 
     * @param bodyIdList
     *            list to be searched in
     * @param id
     *            value of body id
     * @param scope
     *            scope
     * @param type
     *            type
     */
    private void assertContainsBodyId(final List<BodyIdentifier> bodyIdList, 
            final String id, final Scope scope, final Type type) {
        Boolean bodyIdFound = false;
        for (BodyIdentifier bodyId: bodyIdList) {
            Boolean idEquals = false;
            Boolean typeEquals = false;
            Boolean scopeEquals = false;
            
            if (bodyId.getId() == null && id == null 
                    || (bodyId.getId() != null && bodyId.getId().equals(id))) {
                idEquals = true;
            } 
            
            if (bodyId.getType() == null && type == null 
                    || (bodyId.getType() != null && bodyId.getType().equals(type))) {
                typeEquals = true;
            } 
            
            if (bodyId.getScope() == null && scope == null 
                    || (bodyId.getScope() != null && bodyId.getScope().equals(scope))) {
                scopeEquals = true;
            } 
            
            if (idEquals && typeEquals && scopeEquals) {
                bodyIdFound = true;
                break;
            }
        }
        
        assert(bodyIdFound);
    }
}
