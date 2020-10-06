package eu.datlab.worker.global.parsed;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.Jsoup;

import eu.datlab.worker.global.PublicOfficialUtils.EuCountry;
import eu.datlab.worker.parser.BasePublicOfficialParser;
import eu.dl.dataaccess.dto.parsed.ParsedPosition;
import eu.dl.dataaccess.dto.parsed.ParsedPublicOfficial;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Public officials parsed for Rulers.org.
 *
 * @author Michal Riha
 */
public class RulersPublicOfficialParser extends BasePublicOfficialParser {

    private static final String PARSER_VERSION = "1";

    @Override
    public final List<ParsedPublicOfficial> parse(final RawData rawPublicOfficial) {
        final List<ParsedPublicOfficial> parsedPublicOfficials = new ArrayList<>();

        final String[] pageBodyLines = rawPublicOfficial.getSourceData().replaceAll("<HR SIZE=1>", "").split("\\r?\\n");

        String countryCurrentlyParsed = null;
        String regionCurrentlyParsed = null;
        String positionCurrentlyParsed = null;
        boolean nextLineIsSecondLineOfPosition = false;

        for (String line : pageBodyLines) {
            if (startsWith(line, "<title")) {

                countryCurrentlyParsed = parseTextByHtmlTag(line, "title");
            } else if (startsWith(line, "<a")) {

                String tempCountry = parseTextByHtmlTag(line, "a");

                if (tempCountry.length() > 1) {
                    countryCurrentlyParsed = tempCountry;
                    positionCurrentlyParsed = null;
                    regionCurrentlyParsed = null;
                }
            } else if (startsWith(line, "<h2")) {

                regionCurrentlyParsed = parseTextByHtmlTag(line, "h2");
            } else if (countryCurrentlyParsed != null && !EuCountry.contains(countryCurrentlyParsed)) {

                logger.info("Skipping lane not belonging to any EU state: {}", line);
            } else if (nextLineIsSecondLineOfPosition || startsWith(line, "<b")) {

                if (line.endsWith(">") && nextLineIsSecondLineOfPosition) {
                    positionCurrentlyParsed += line;
                    positionCurrentlyParsed = parseTextByHtmlTag(positionCurrentlyParsed, "b");
                    nextLineIsSecondLineOfPosition = false;
                } else if (line.endsWith(">")) {
                    positionCurrentlyParsed = parseTextByHtmlTag(line, "b");
                } else {
                    positionCurrentlyParsed += line;
                    nextLineIsSecondLineOfPosition = true;
                }
            } else if (line.length() < 28) {

                logger.warn("Line looks like public official, but is not long enough {}", line);
            } else if (Character.isDigit(line.trim().charAt(0)) && positionCurrentlyParsed != null) {

                String publicOfficialDateOfBirth = null;
                String publicOfficialDateOfDeath = null;
                String positionStartDate = line.substring(0, 11).trim();
                String positionEndDate = line.substring(14, 25).trim();

                // parse name and dates of birth and death
                String publicOfficialNameAndLife = line.substring(27);
                if (startsWith(publicOfficialNameAndLife, "<a")) {
                    publicOfficialNameAndLife = parseTextByHtmlTag(publicOfficialNameAndLife, "a");
                }
                String[] publicOfficialNameAndLifeArray = publicOfficialNameAndLife.split("\\(");

                String publicOfficialName = publicOfficialNameAndLifeArray[0].trim();

                if (publicOfficialNameAndLifeArray.length > 1 && publicOfficialNameAndLifeArray[1].contains("-")) {
                    String[] publicOfficialLifeArray = publicOfficialNameAndLifeArray[1].replace(")", "").split("-");

                    if (publicOfficialLifeArray.length > 1) {
                        publicOfficialDateOfBirth = publicOfficialLifeArray[0].replace("b.", "").trim();
                        publicOfficialDateOfDeath = publicOfficialLifeArray[1].replace("d.", "").trim();
                    }
                } else if (publicOfficialNameAndLifeArray.length > 1 && publicOfficialNameAndLifeArray[1].contains(
                        "b.")) {
                    publicOfficialDateOfBirth = publicOfficialNameAndLifeArray[1].replace("b.", "")
                            .replace(")", "")
                            .trim();
                }

                parsedPublicOfficials.add(new ParsedPublicOfficial().setFullName(publicOfficialName)
                        .setDateOfBirth(publicOfficialDateOfBirth)
                        .setDateOfDeath(publicOfficialDateOfDeath)
                        .setCountry(countryCurrentlyParsed)
                        .setRegion(regionCurrentlyParsed)
                        .addPosition(new ParsedPosition().setPosition(positionCurrentlyParsed)
                                .setStartDate(positionStartDate)
                                .setEndDate(positionEndDate)));
            }
        }

        System.out.println("");

        return parsedPublicOfficials;
    }

    @Override
    protected final String getVersion() {
        return PARSER_VERSION;
    }

    /**
     * Checks if string starts with given string, ignores spaces and is case insensitive.
     *
     * @param string
     *         string to check from
     * @param keyWord
     *         string to check by
     *
     * @return true or false
     */
    private boolean startsWith(final String string, final String keyWord) {
        return string.toLowerCase().trim().startsWith(keyWord);
    }

    /**
     * Parses text from given string by html jsoupSelector, using Jsoup selector.
     *
     * @param string
     *         to parse from
     * @param jsoupSelector
     *         jsoupSelector to parse by
     *
     * @return parsed text
     */
    private String parseTextByHtmlTag(final String string, final String jsoupSelector) {
        return Jsoup.parse(string).select(jsoupSelector).text();
    }

    @Override
    protected final List<ParsedPublicOfficial> postProcessSourceSpecificRules(final List<ParsedPublicOfficial> parsed, final RawData raw) {
        return parsed;
    }
}
