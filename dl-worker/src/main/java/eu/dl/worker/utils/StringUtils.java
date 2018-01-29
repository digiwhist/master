package eu.dl.worker.utils;

import static org.apache.commons.lang3.StringUtils.lastOrdinalIndexOf;
import static org.apache.commons.lang3.StringUtils.leftPad;
import static org.apache.commons.lang3.StringUtils.ordinalIndexOf;
import static org.apache.commons.lang3.StringUtils.right;
import static org.apache.commons.lang3.StringUtils.strip;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Predicate;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Class provides useful functions for string processing.
 *
 * @author Tomas Mrazek
 */
public final class StringUtils {

    private static final Logger logger = LoggerFactory.getLogger(StringUtils.class);
    
    /**
     * Supress default constructor for noninstantiability.
     */
    private StringUtils() {
        throw new AssertionError();
    }

    /**
     * Splits the {@code input} string to the list of strings. These strings haven't more than {@code linesCount} lines.
     * Also strips new line character from the start and end of a String. Additionaly filters rows if the {@code filter}
     * is defined.
     *
     * @param str
     *      string for splitting
     * @param n
     *      maximal number of lines in chunk
     * @param skip
     *      count of skipped rows
     * @param filter
     *      row filter,
     * @return list of chunks
     * @throws IOException
     *      If an I/O error occurs
     */
    public static List<String> chunkStringByLines(final String str, final int n, final int skip,
        final Predicate<String> filter) throws IOException {

        if (str == null || str.isEmpty() || n <= 0) {
            return Collections.emptyList();
        }

        final List<String> chunks = new ArrayList<>();
        final BufferedReader br = new BufferedReader(new StringReader(str));
        StringBuilder sb = new StringBuilder();
        try {
            if (skip > 0) {
                br.skip(ordinalIndexOf(str, "\n", skip) + 1);
            }
            String line;
            int lineNumber = 1;
            while ((line = br.readLine()) != null) {
                if (line.isEmpty() || (filter != null && !filter.test(line))) {
                    continue;
                }

                sb.append(line).append("\n");
                if (lineNumber % n == 0) {
                    chunks.add(strip(sb.toString(), "\n"));
                    sb = new StringBuilder();
                }

                lineNumber++;
            }
        } catch (IOException e) {
            throw e;
        } finally {
            if (sb.length() > 0) {
                chunks.add(strip(sb.toString(), "\n"));
            }
            try {
                br.close();
            } catch (IOException e) {
                logger.error("BufferedReader closing failed because of", e);
            }
        }
        
        return chunks;
    }

    /**
     * Splits the {@code input} string to the list of strings. These strings haven't more than {@code linesCount} lines.
     * Also strips new line character from the start and end of a String.
     *
     * @param str
     *      string for splitting
     * @param n
     *      maximal number of lines in chunk
     * @param skip
     *      count of skipped rows
     * @return list of chunks
     * @throws IOException
     *      If an I/O error occurs
     */
    public static List<String> chunkStringByLines(final String str, final int n, final int skip) throws IOException {
        return chunkStringByLines(str, n, skip, null);
    }

    /**
     * Returns last {@code n} lines from the given input string {@code str}. In case of an empty/null string or
     * {@code n <= 0} returns null. If {@code n} is bigger than a number of lines of the input string returns whole
     * input string. Also strips new line character from the start and end of a String.
     *
     * @param str
     *      input string
     * @param n
     *      number of lines
     * @return last n lines of the input string or null
     */
    public static String tail(final String str, final int n) {
        if (str == null || str.isEmpty() || n <= 0) {
            return null;
        }

        int index = lastOrdinalIndexOf(str, "\n", n);

        return index == -1 ? str : strip(str.substring(index + 1), "\r\n");
    }

    /**
     * Returns first {@code n} lines from the given input string {@code str}. In case of an empty/null string or
     * {@code n <= 0} returns null. If {@code n} is bigger than a number of lines of the input string returns whole
     * input string. Also strips new line character from the start and end of a String.
     *
     * @param str
     *      input string
     * @param n
     *      number of lines
     * @return first n lines of the inout string
     */
    public static String head(final String str, final int n) {
        if (str == null || str.isEmpty() || n <= 0) {
            return null;
        }
        
        int index = ordinalIndexOf(str, "\n", n);

        return index == -1 ? str : strip(str.substring(0, index), "\r\n");
    }

    /**
     * Splits the given string {@code str} by the given {@code regex}. In case that str is null or empty returns empty
     * list (note {@link java.lang.String#split(java.lang.String)} in case of empty string returns list that includes
     * one element, empty string).
     *
     * @param str
     *      string to be split
     * @param regex
     *      delimiting regular expression
     * @return the list of strings computed by splitting this string around matches of the given regular expression
     */
    public static List<String> split(final String str, final String regex) {
        if (str == null || str.isEmpty()) {
            return Collections.emptyList();
        }

        return Arrays.asList(str.split(regex));
    }

    /**
     * Returns string with given length. In case that the {@code str} is longer than the given {@code length} returns
     * given number of the rightmost characters, for shorter string returns left padded string (by the given
     * {@code padStr}), otherwise input str.
     *
     * @param str
     *      string to be justified
     * @param length
     *      length of the result string
     * @param padStr
     *      the String to pad with, null or empty treated as single space
     * @return justified string or null
     */
    public static String justifyLeft(final String str, final int length, final String padStr) {
        return leftPad(right(str, length), length, padStr);
    }

    /**
     * Removes dots at the end of input string. The string is trimmed to remove spaces and get the dots at the end.
     *
     * @param string
     *         string to remove dots
     *
     * @return trimmed input string without dots and spaces at the end of input string
     */
    public static String removeDotsAtTheEnd(final String string) {
        if (string == null) {
            return null;
        }

        String stringToModify = string;

        boolean wasStringModified;
        // while loop is necessary, because the string can be " , à 17 heures . ."
        do {
            if (stringToModify.endsWith(" ")) {
                stringToModify = stringToModify.trim();
                wasStringModified = true;
            } else if (stringToModify.endsWith(".")) {
                stringToModify = stringToModify.substring(0, stringToModify.length() - 1);
                wasStringModified = true;
            } else {
                wasStringModified = false;
            }
        } while (wasStringModified);

        return stringToModify;
    }

}
