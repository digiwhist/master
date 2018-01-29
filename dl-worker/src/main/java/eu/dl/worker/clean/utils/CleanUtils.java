package eu.dl.worker.clean.utils;


import eu.dl.dataaccess.dto.parsed.Parsable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * Provides method for cleaning purposes.
 * 
 * @author Tomas Mrazek
 */
public final class CleanUtils {
    /**
     * Supress default constructor for noninstantiability.
     */
    private CleanUtils() {
        
    }

    /**
     * @param parsable
     *      parsable item
     * @return country of the parsable item based on createdBy value (worker name)
     */
    public static String getParsedItemCountry(final Parsable parsable) {
        Matcher m = Pattern.compile("(?<=worker\\.)(?<country>[^\\.]+)(?=\\.parsed)").matcher(parsable.getCreatedBy());
        return m.find() ? m.group("country") : null;
    }
}
