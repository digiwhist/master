package eu.datlab.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.worker.utils.StringUtils;
import org.jsoup.nodes.Element;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Utilities for ENarocanje.
 *
 * @author Marek Mikes
 */
final class ENarocanjeTenderFormUtils {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeTenderFormUtils() {
    }

    static final String IS_EU_FUND_TITLE =
            "Naročilo se nanaša na projekt in/ali program, ki se financira s sredstvi EU:";
    static final String DURATION_IN_DAYS_TITLE = "Trajanje v dnevih:";
    static final String DURATION_IN_DAYS_SUFFIX = "(od oddaje naročila)";

    /**
     * Decides whether input string represents "yes" or "no" in Slovenian language.
     *
     * @param yesOrNoString
     *         string which represents "yes" or "no" in Slovenian language
     *
     * @return true when input string represents "yes" in Slovenian language;
     *         false when input string represents "no" in Slovenian language;
     *         otherwise null
     */
    static Boolean meansYes(final String yesOrNoString) {
        if (yesOrNoString == null) {
            return null;
        }

        String yesOrNoStringCleaned = StringUtils.removeDotsAtTheEnd(yesOrNoString);

        if (yesOrNoStringCleaned.isEmpty()) {
            return null;
        }

        switch (yesOrNoStringCleaned.trim()) {
            case "Da":
            case "da":
                return true;
            case "Ne":
            case "ne":
                return false;
            default:
                assert false;
                return null;
        }
    }

    /**
     * Parses name and address of body.
     *
     * @param nameAndAddress
     *          name and address in string.
     * @param body
     *          body to be updated, if null the new body will be created
     * @return parsed body or null
     */
    static ParsedBody parseNameAndAddressBody(final String nameAndAddress, final ParsedBody body) {
        if (nameAndAddress == null || nameAndAddress.isEmpty()) {
            return body;
        }

        ParsedBody newBody = Optional.ofNullable(body).orElse(new ParsedBody());

        final List<String> separators = Arrays.asList(
            "d.o.o.,", "D.O.O.,", "d.o.o. ,", "d. o. o.,", "d.o.o. ",
            "d.d.,", "d.d,",
            "s.p.,", "S.P.,", "s.p. ,",
            ",");

        String separator = separators.stream().filter(s -> nameAndAddress.contains(s)).findFirst().orElse(null);
        if (separator == null) {
            return newBody.setName(nameAndAddress);
        }

        int addressStartIndex = nameAndAddress.indexOf(separator) + separator.length();

        return newBody
            .setName(nameAndAddress.substring(0, addressStartIndex))
            .setAddress(new ParsedAddress().setRawAddress(nameAndAddress.substring(addressStartIndex)));
    }

    /**
     * Splits HTML of the given node by {@code <br>}.
     * @param node
     *      node to be split
     * @return list of strings or empty list
     */
    static List<String> splitByBR(final Element node) {
        if (node == null || node.html().isEmpty()) {
            return Collections.emptyList();
        }

        return Arrays.asList(node.html().split("<br>"));
    }
}
