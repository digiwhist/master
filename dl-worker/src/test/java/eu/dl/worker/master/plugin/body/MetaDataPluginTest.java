package eu.dl.worker.master.plugin.body;

import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.HashMap;

import org.junit.Test;

import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.worker.master.plugin.MasterPlugin;

/**
 * Test of bodyidmastering plugin.
 */
public class MetaDataPluginTest {
    private MatchedBody nullBody1 = new MatchedBody();
    
    private MatchedBody nullBody2 = new MatchedBody();
    
    private MatchedBody okBody1 = new MatchedBody();
    
    private MatchedBody okBody2 = new MatchedBody();
    
    /**
     * Null values test.
     */
    @Test
    public final void nullTest() {
        MasterPlugin masterPlugin = new MetaDataPlugin();
        
        MasterBody masterBody = new MasterBody();
        masterPlugin.master(Arrays.asList(nullBody1, nullBody2), masterBody, null);
        assertTrue(masterBody.getMetaData() == null);
        
        nullBody2.setMetaData(null);
        masterBody = new MasterBody();
        masterPlugin.master(Arrays.asList(nullBody1, nullBody2), masterBody, null);
        assertTrue(masterBody.getMetaData() == null);
    }
    
    /**
     * Null values test.
     */
    @Test
    public final void okTest() {
        MasterPlugin masterPlugin = new MetaDataPlugin();
        
        HashMap<String, Object> okData1 = new HashMap<String, Object>();
        okData1.put("key1", "aaa");
        
        HashMap<String, Object> okData2 = new HashMap<String, Object>();
        okData2.put("key2", "bbb");
        
        okBody1.setMetaData(okData1);
        okBody2.setMetaData(okData2);
        
        // no ids at all
        MasterBody masterBody = new MasterBody();
        masterPlugin.master(Arrays.asList(okBody1, okBody2), masterBody, null);
        
        assertTrue(masterBody.getMetaData().get("key1").equals("aaa"));
        assertTrue(masterBody.getMetaData().get("key2").equals("bbb"));
    }
}
