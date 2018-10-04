package eu.dl.worker.clean.utils;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.safety.Whitelist;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class holds methods handy for string cleaning.
 *
 * @author Kuba Krafka
 */
public final class StringUtils {
    private static final Logger logger = LoggerFactory.getLogger(StringUtils.class.getName());

    /**
     * Minimum length of the body id.
     */
    private static final int BODYID_MIN_LENGTH = 3;
    /**
     * Maximum length of the body id.
     */
    private static final int BODYID_MAX_LENGTH = 100;

    /**
     * Utility class should not have public constructor.
     */
    private StringUtils() {
    }

    /**
     * Method to clean short text such a title, street or similar. It does trim,
     * beautify whitespace, removes html etc.
     *
     * @param value
     *            value to be cleaned
     *
     * @return cleaned value
     */
    public static String cleanShortString(final String value) {
        final String valueForCleaning = StringUtils.prepareStringForCleaning(value);
        if (valueForCleaning == null || valueForCleaning.isEmpty()) {
            return null;
        }

        logger.debug("Cleaning short string \"{}\"", value);

        // remove html
        String cleaned = Jsoup.parse(valueForCleaning).text();

        // remove trailing whitespace
        cleaned = cleaned.trim();

        // Remove html
        cleaned = Jsoup.clean(cleaned, "", Whitelist.none(), new Document.OutputSettings().prettyPrint(false));
        cleaned = StringEscapeUtils.unescapeHtml4(cleaned);

        // replace double whitespace in a row
        // and replace end lines. tabs etc. with space
        cleaned = cleaned.replaceAll("\\s+", " ");

        if (cleaned.isEmpty()) {
            return null;
        } else {
            return cleaned;
        }
    }

    /**
     * Method to clean long texts such a descriptions. It does trim, beautify
     * whitespaces, removes html(but keeps end lines) etc.
     *
     * @param value
     *            value to be cleaned
     *
     * @return cleaned value
     */
    public static String cleanLongString(final String value) {
        final String valueForCleaning = StringUtils.prepareStringForCleaning(value);
        if (valueForCleaning == null || valueForCleaning.isEmpty()) {
            return null;
        }

        logger.debug("Cleaning long string \"{}\"", value);

        String cleaned = StringUtils.cleanHtml(valueForCleaning);

        if (cleaned.isEmpty()) {
            return null;
        } else {
            return cleaned;
        }
    }

    /**
     * Capitalize first letter in the string.
     *
     * @param value
     *            processed value
     *
     * @return value with first letter capitalized
     */
    public static String upperCaseFirst(final String value) {
        if (value == null) {
            return null;
        }

        logger.debug("Capitalize first letter in the string \"{}\"", value);

        char[] array = value.toCharArray();
        array[0] = Character.toUpperCase(array[0]);

        return new String(array);
    }

    /**
     * Lower all letters in the string.
     *
     * @param value
     *            processed value
     *
     * @return value with first letter capitalized
     */
    public static String lowerCase(final String value) {
        if (value == null) {
            return null;
        }

        logger.debug("Lower all letters in the string \"{}\"", value);

        return value.toLowerCase();
    }

    /**
     * Transform html text to plain text.
     *
     * @param html
     *            text to be processed
     * @return clean text
     */
    public static String cleanHtml(final String html) {
        if (html == null || html.trim().isEmpty()) {
            return null;
        }

        String result = "";

        Document document = Jsoup.parse(html.trim());

        // check whether its html att all
        if (document.text().trim().equals(html.trim())) {
            // not html, return original string
            return html.trim();
        }

        // makes html() preserve linebreaks and spacing
        document.outputSettings(new Document.OutputSettings().prettyPrint(false));

        // trick to keep endlines
        document.select("br").append("\\n");
        document.select("p").append("\\n\\n");
        document.select("ul").prepend("\\n");
        document.select("ul").append("\\n");
        document.select("li").append("\\n");

        // wrong endlines
        result = document.html().replaceAll("\\\\n", "\n");

        result = Jsoup.clean(result, "", Whitelist.none(), new Document.OutputSettings().prettyPrint(false));

        result = StringEscapeUtils.unescapeHtml4(result);

        return result.trim();
    }

    /**
     * Transform String to Boolean. For transformation is used method Boolean.valueOf(...) which returns Boolean
     * representing a true value only, and only if that string argument is not null and is equal, ignoring case, to
     * the string "true".
     *
     * @param bool
     *      transformed string
     * @return Boolean or null if {@code bool} parameter is null or empty
     */
    public static Boolean cleanBoolean(final String bool) {
        final String boolForCleaning = StringUtils.prepareStringForCleaning(bool);
        if (boolForCleaning == null || boolForCleaning.isEmpty()) {
            return null;
        }
        logger.debug("Cleaning boolean string \"{}\"", bool);
        return BooleanUtils.toBoolean(boolForCleaning);
    }

    /**
     * Transform String to URL. Preppends url scheme if is defined.
     *
     * @param url
     *      cleaned url string
     * @param scheme
     *      default url scheme in case that parsing of the URL fails because of "no protocol" exception
     * @return URL or null if {@code url} parameter is null or empty
     */
    public static URL cleanURL(final String url, final URLSchemeType scheme) {
        String urlForCleaning = StringUtils.cleanShortString(url);
        if (urlForCleaning == null || urlForCleaning.isEmpty()) {
            return null;
        }

        logger.debug("Cleaning URL string \"{}\"", url);

        URL cleanUrl;
        try {
            cleanUrl = new URL(urlForCleaning);

            // attempt to get URL host, if it's not empty returns URL (unfortunately eg. calling `new URL("https:")`
            // doesn't throw an exception but returns new URL instance), otherwise continue in cleaning process.
            if (!cleanUrl.getHost().isEmpty()) {
                return cleanUrl;
            }
        } catch (MalformedURLException e) {
            // something in the URL is wrong. We will try to repair it
        }

        //common typos repair
        final Map<String, String> regexpsToFix = new HashMap<>();
        regexpsToFix.put("(?i)www:", "www.");
        regexpsToFix.put("(?i)^[a-z]*\\.?(h{1,}tt?p?)(s)?:", "http$2:");
        regexpsToFix.put("(?i)^www\\.http", "http");
        regexpsToFix.put("(?i)^(https?):?/{1,}", "$1://");
        regexpsToFix.put("(?i)^httphttp", "https");
        regexpsToFix.put("(?i)^h[a-z]*ttp(s)?", "http$1");
        regexpsToFix.put("(?i)^http://http:", "http://");
        regexpsToFix.put("(?i)^(https?)://:www", "$1://www");
        regexpsToFix.put("(?i)^.+(https?)://", "$1://");
        regexpsToFix.put("(?i)^ttps", "https");
        regexpsToFix.put("(?i)^htpp", "http");
        regexpsToFix.put("(?i)^(https?):(?<!//)", "$1://");
        regexpsToFix.put("^:", "");

        for (Map.Entry<String, String> entry : regexpsToFix.entrySet()) {
            String urlForCleaningByRegexp = urlForCleaning;
            try {
                urlForCleaningByRegexp = urlForCleaningByRegexp.replaceFirst(entry.getKey(), entry.getValue());
                cleanUrl = new URL(urlForCleaningByRegexp);
                
                // attempt to get URL host, if it's not empty returns URL (unfortunately eg. calling `new URL("https:")`
                // doesn't throw an exception but returns new URL instance), otherwise continue in cleaning process.
                if (!cleanUrl.getHost().isEmpty()) {
                    return cleanUrl;
                }
            } catch (MalformedURLException e1) {
                if (scheme != null && e1.getMessage().contains("no protocol")) {
                    try {
                        // on this place is not necessary to resolve empty host. For this case, the given URL has no set
                        // the protocol.
                        return new URL(scheme.getScheme() + urlForCleaningByRegexp);
                    } catch (MalformedURLException e2) {
                        // no operation - just try another regexp
                    }
                }
            }
        }

        logger.error("Cleaning failed - cleaning URL string \"{}\" failed.", url);
        return null;
    }

    /**
     * Transform String to URL.
     *
     * @param url
     *      cleaned url string
     * @return URL or null if {@code url} parameter is null or empty
     */
    public static URL cleanURL(final String url) {
        return StringUtils.cleanURL(url, null);
    }

    /**
     * Method to clean body identifier text.
     * E.g. this body identifier "36 183 792" is cleaned to "36183792".
     *
     * @param value
     *            value to be cleaned
     *
     * @return cleaned value or null in case that the identifier is shorter than {@link StringUtils#BODYID_MIN_LENGTH}
     *          or longer than {@link StringUtils#BODYID_MAX_LENGTH}
     */
    public static String cleanBodyIdentifier(final String value) {
        String cleaned = cleanShortString(value);
        if (cleaned == null) {
            return null;
        }

        // delete all whitespaces
        cleaned = cleaned.replaceAll("\\s+", "");

        return BODYID_MIN_LENGTH <= cleaned.length() && cleaned.length() <= BODYID_MAX_LENGTH ? cleaned : null;
    }

    /**
     * Replaces all occurrences of the Unicode speces with ordinary space (\\u0020).
     *
     * @param input
     *      string to be cleaned
     * @return cleaned string or null
     */
    public static String clearUnicodeSpaces(final String input) {
        if (input == null) {
            return null;
        }
        
        return input
            /*
            covers unicode non-zero width spaces (\u0020, \u00A0, \u1680, \u180E, \u2000, \u2001, \u2002, \u2003,
                \u2004, \u2005, \u2006, \u2007, \u2008, \u2009, \u200A, \u202F, \u205F, \u3000)
            */
            .replaceAll("\\h", " ")
            /*
            covers unicode zero width spaces (\u200B, \uFEFF)
            */
            .replaceAll("\u200B|\uFEFF", " ");
    }

    /**
     * Replaces all occurrences of the invisible control characters and unused code points. These are:
     *  <ol>
     *      <li>an ASCII or Latin-1 control character: 0x00–0x1F and 0x7F–0x9F</li>
     *      <li>an invisible formatting indicator</li>
     *      <li>any code point reserved for private use</li>
     *      <li>one half of a surrogate pair in UTF-16 encoding</li>
     *      <li>any code point to which no character has been assigned</li>
     *  </ol>
     *
     * @param input
     *      input string to be cleaned
     * @return cleaned string or null
     */
    public static String cleanUnicodeInvisibleCharacters(final String input) {
        if (input == null) {
            return null;
        }

        return input.replaceAll("\\p{C}", "");
    }

    /**
     * Prepares {@code input} string for cleaning. The process consists from two steps:
     * <ol>
     *  <li>cleans Unicode spaces ({@link StringUtils#clearUnicodeSpaces(java.lang.String)})</li>
     *  <li>trims string ({@link String#trim()})</li>
     * </ol>
     *
     * @param input
     *      string to be prepared
     * @return string or null
     */
    public static String prepareStringForCleaning(final String input) {
        if (input == null) {
            return null;
        }

        return cleanUnicodeInvisibleCharacters(clearUnicodeSpaces(input)).trim();
    }
}
