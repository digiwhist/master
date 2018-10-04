package eu.dl.utils.nuts;

/**
 *
 * @author Tomas Mrazek
 */
public class NutsServiceFacotry {
    
    private static NutsService instance;

    /**
     * Supress default constructor for noninstatiability.
     */
    protected NutsServiceFacotry() {
    }

    /**
     * @return NutsService instance
     */
    public static NutsService getNutsService(){
        if (instance == null) {
            instance = new BasicNutsService();
        }
        
        return instance;
    }
}
