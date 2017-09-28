package eu.dl.dataaccess.utils;

import java.util.ArrayList;
import java.util.List;

import eu.dl.dataaccess.dto.codetables.BodyType;
import eu.dl.dataaccess.dto.matched.MatchedBody;

/**
 * Class provides functions for body manipulation.
 */
public final class BodyUtils {
    
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    private BodyUtils() {
    }
 
    /**
     * Returns list of all field names containing bodies. 
     * For example "bidders", "administrators", "onBehalfOf" etc.
     * 
     * @return list with body fields
     */
    public static List<String> getBodyFieldNames() {
        List<String> result = new ArrayList<String>(); 
        for (BodyType bodyType : BodyType.values()) {
            result.add(toCamelCase(bodyType.name()));
        }
        
        return result;
    }
    
    /**
     * Returns list of all field names containing bodies. 
     * For example "bidders", "administrators", "onbehalfof" etc.
     * 
     * @return list with body fields
     */
    public static List<String> getBodyFieldNamesInLowerCase() {
        List<String> result = new ArrayList<String>(); 
        for (BodyType bodyType : BodyType.values()) {
            result.add(toCamelCase(bodyType.name()).toLowerCase());
        }
        
        return result;
    }
    
    /**
     * This method picks up the most complete body from the list of bodies. It
     * compares bodies based on name, bodyid and address. 
     * 
     * 
     * @param bodies
     *            the result is selected from this bodies
     * 
     * @return the most complete body or null in case input is empty or null
     */
    public static MatchedBody getMostCompleteBody(final List<MatchedBody> bodies) {
        if (bodies == null || bodies.isEmpty()) {
            return null;
        }
        
        MatchedBody result = null;
        Double winningScore = new Double(0);

        for (MatchedBody body : bodies) {
            // calculate score for current body
            Double actualScore = body.getCompletenessScore();

            if (actualScore == null) {
                actualScore = 0.0;
            }

            if (actualScore >= winningScore) {
                // and we have found a new candidate...
                winningScore = actualScore;
                result = body;
            }
        }
        
        return result;
    }

    /**
     * Calculates completeness score for the body. The completeness is
     * calculated as sum of filled fields, where non-empty name = 1.1, non-empty
     * address = 0.9 and non-empty body.bodyids = 1. The body with highest
     * completeness score is the most complete one.
     * 
     * @param body
     *            the completeness will be calculated for this body
     * @return completeness score(0 for null values)
     */
    public static Double completenessScore(final MatchedBody body) {
        Double score = new Double(0);

        if (body == null) {
            return score;
        }

        if (body.getName() != null && !body.getName().isEmpty()) {
            score = score + 1.1;
        }

        if (body.getAddress() != null) {
            score = score + 0.9;
        }

        if (body.getBodyIds() != null && !body.getBodyIds().isEmpty()) {
            score = score + 1;
        }

        return score;
    }

    /**
     * Transforms THIS_STRING to thisString.
     * 
     * @param underscore
     *            string to be transformed
     * 
     * @return transformed string
     */
    private static String toCamelCase(final String underscore) {
        if (underscore == null) {
            return null;
        }
        
        // split by underscore and process each part separately
        String[] parts = underscore.split("_");
        String camelCaseString = "";
        for (String part : parts){
            camelCaseString = camelCaseString + part.substring(0, 1).toUpperCase() + part.substring(1).toLowerCase();
        }
        
        return camelCaseString;
    }
}
