package eu.dl.dataaccess.dto.codetables;

import org.apache.commons.lang3.text.WordUtils;

/**
 * Utils class for OCDS enumerations.
 *
 * @author Tomas Mrazek
 */
public final class OCDSEnumUtils {
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    private OCDSEnumUtils() {
    }
    
    /**
     * Transmutes an enumerate name to format of OCDS codelist item (camel case).
     *
     * @param e
     *      item whose name will be transmuted
     * @return name in OCDS format
     */
    public static String ocdsCodelistJsonValue(final Enum e) {
        // capitalize first letter of each word separated by '_', then remove '_'
        String ocds = WordUtils.capitalizeFully(e.name(), new char[]{'_'}).replace("_", "");

        // first letter is small
        char[] array = ocds.toCharArray();
        array[0] = Character.toLowerCase(array[0]);

        return new String(array);
    }
}
