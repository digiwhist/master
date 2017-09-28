package eu.digiwhist.worker.ge;

import org.jsoup.nodes.Element;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Common utilities for Georgia.
 *
 * @author Marek Mikes
 */
public final class SPATenderUtils {
    /**
     * Private constructor to make this class static.
     */
    private SPATenderUtils() {}

    /**
     * Parses subject id from onlick parameter of given element.
     *
     * @param element
     *         element with onclick attribute
     *
     * @return id of the subject
     */
    public static String getSubjectId(final Element element) {
        if (element == null) {
            return null;
        }

        Pattern r = Pattern.compile("ShowProfile\\(([^\\(\\)]+)\\)");
        Matcher m = r.matcher(element.attr("onclick"));
        return m.find() ? m.group(1) : null;
    }
}
