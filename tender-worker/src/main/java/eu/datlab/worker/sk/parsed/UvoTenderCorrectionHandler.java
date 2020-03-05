package eu.datlab.worker.sk.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.cz.parsed.VestnikTenderParserUtils;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedCorrigendum;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.TextNode;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDate;
import java.time.Month;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static eu.datlab.worker.sk.parsed.UvoTenderParserUtils.getFirstValueFromElement;

/**
 * Tender corrections form handler for Slovakia.
 *
 * @author Tomas Mrazek
 */
final class UvoTenderCorrectionHandler {
    private static final Logger logger = LoggerFactory.getLogger(VestnikTenderParserUtils.class);

    private static final Pattern SECTION_REGEX = Pattern.compile("^(?<section>[IVX]+(\\.[1-9a-z]+)*)(\\)|\\.)");

    private static final String DATE_REGEX = "^\\d{2}\\.\\d{2}\\.\\d{4}( \\d{2}:\\d{2})?";

    private static final String ORIGINAL_TITLE = "Namiesto:";

    private static final Pattern REPLACEMENT_TITLE_REGEX = Pattern.compile("^(Má byť|Viď):");

    private static final Pattern CORRECTION_TITLE_REGEX = Pattern.compile("^(?<sid>\\d+) - .+ (?<day>\\d+)/(?<year>\\d{4})");

    private static final List<Pattern> BLACK_LIST_REGEX = Arrays.asList(
        Pattern.compile("OPRAVA DÁTUMU"),
        Pattern.compile("Miesto, kde má byť dátum upravený:$")
    );

    /**
     * Suppress default constructor.
     */
    private UvoTenderCorrectionHandler() {
    }

    /**
     * Parse correction form.
     *
     * @param document        document to parse data from
     * @param url             url of the document
     * @param publicationDate publication date of the document
     *
     * @return List<ParsedTender> with parsed data
     */
    public static List<ParsedTender> parse(final Document document, final String url, final String publicationDate) {
        // included publication
        final ParsedTender parsedTender = new ParsedTender()
            .addPublication(new ParsedPublication()
                .setSource(PublicationSources.SK_UVO)
                .setPublicationDate(publicationDate)
                .setSourceId(parsePublicationSourceId(document))
                .setSourceFormType(parsePublicationSourceFormType(document))
                .setHumanReadableUrl(url)
                .setIsIncluded(true)
                .setLanguage("SK"));

        // non-included publication for particular corrections
        final Elements publications = JsoupUtils.select("div[id=corrections] tr > th > a", document);
        for (Element p : publications) {
            ParsedPublication parsedPublication = new ParsedPublication()
                .setSource(PublicationSources.SK_UVO)
                .setSourceFormType(PublicationFormType.CONTRACT_UPDATE.name())
                .setHumanReadableUrl(PublicationSources.SK_UVO + p.attr("href"))
                .setIsIncluded(false);

            Matcher m = CORRECTION_TITLE_REGEX.matcher(p.text());
            if (m.find()) {
                parsedPublication
                    .setSourceId(m.group("sid"))
                    .setPublicationDate(getPublicationDate(Integer.valueOf(m.group("day")), Integer.valueOf(m.group("year"))).toString());
            }

            parsedTender.addPublication(parsedPublication);
        }

        // corrections data
        final Elements corrections = JsoupUtils.select("div[id=corrections] tr > td", document);

        for (Element c : corrections) {
            final List<TextNode> nodes = c.textNodes();

            if (nodes != null && !nodes.isEmpty()) {
                final List<String> strings = nodes.stream()
                    .map(n -> n.text().trim())
                    .filter(n -> !n.isEmpty() && BLACK_LIST_REGEX.stream().noneMatch(r -> r.matcher(n).find()))
                    .map(n -> n.replace("Miesto, kde má byť dátum upravený: ", ""))
                    .map(n -> n.replace("Oddiel: ", ""))
                    .collect(Collectors.toList());

                ParsedCorrigendum correction = null;
                boolean isOriginalParsed = true;
                String original = "", replacement = "", section = null;
                for (String s : strings) {
                    Matcher replacementMatcher = REPLACEMENT_TITLE_REGEX.matcher(s);

                    // initialization of new correction and saving of correction from previous iteration
                    if (isStartOfSection(s, section)) {
                        if (correction != null) {
                            if (original.matches(DATE_REGEX)) {
                                parsedTender.addCorrigendum(correction.setOriginalDate(original).setReplacementDate(replacement));
                            } else {
                                parsedTender.addCorrigendum(correction.setOriginal(original).setReplacement(replacement));
                            }
                        }

                        original = "";
                        replacement = "";
                        // always matches (see. isStartOfSection), but section number is needed
                        Matcher sectionMatcher = SECTION_REGEX.matcher(s);
                        if (sectionMatcher.find()) {
                            section = sectionMatcher.group("section");
                        }

                        correction = new ParsedCorrigendum().setSectionNumber(s);
                    } else if (s.startsWith(ORIGINAL_TITLE)) {
                        isOriginalParsed = true;
                        // value is on the same line as title
                        String value = s.replace(ORIGINAL_TITLE, "").trim();
                        if (!value.isEmpty()) {
                            original = value;
                        }
                    } else if (replacementMatcher.find()) {
                        isOriginalParsed = false;
                        // value is on the same line as title
                        String value = s.replaceAll(REPLACEMENT_TITLE_REGEX.pattern(), "").trim();
                        if (!value.isEmpty()) {
                            replacement = value;
                        }
                    // original/replacement value parsing (until after first initialization)
                    } else if (correction != null) {
                        if (isOriginalParsed) {
                            original += (original.isEmpty() ? "" : "\n") + s;
                        } else {
                            replacement += (replacement.isEmpty() ? "" : "\n") + s;
                        }
                    }
                }

                if (correction != null) {
                    if (original.matches(DATE_REGEX)) {
                        parsedTender.addCorrigendum(correction.setOriginalDate(original).setReplacementDate(replacement));
                    } else {
                        parsedTender.addCorrigendum(correction.setOriginal(original).setReplacement(replacement));
                    }
                }
            }
        }

        return Collections.singletonList(parsedTender);
    }

    /**
     * @param days
     *      number of workdays days in year
     * @param year
     *      year
     * @return date
     */
    private static LocalDate getPublicationDate(final int days, final int year) {
        return LocalDate.of(year, Month.JANUARY, 1).plus(days + (days/5)*2, ChronoUnit.DAYS);
    }

    /**
     * @param s
     *      string to be tested
     * @param section
     *      current section code/number
     * @return TRUE if and only if the give string is start of section
     */
    private static boolean isStartOfSection(final String s, final String section) {
        Matcher sectionMatcher = SECTION_REGEX.matcher(s);
        if (!sectionMatcher.find()) {
            return false;
        } else if (section == null || section.isEmpty()) {
            return true;
        }

        String newSection = sectionMatcher.group("section");

        if (newSection.startsWith(section)) {
            // same section can be modified multiple times
            return newSection.equals(section);
        }

        return true;
    }

    /**
     * Parse publication source ID value from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private static String parsePublicationSourceId(final Document document) {
        String[] parsedPublicationSourceInfo = parsePublicationSourceInfo(document);

        if (parsedPublicationSourceInfo != null && parsedPublicationSourceInfo.length >= 1) {
            return parsedPublicationSourceInfo[0].trim();
        }

        return null;
    }

    /**
     * Parse form type of publication source from document.
     *
     * @param document document to be parsed
     *
     * @return String or Null
     */
    private static String parsePublicationSourceFormType(final Document document) {
        String[] parsedPublicationSourceInfo = parsePublicationSourceInfo(document);

        if (parsedPublicationSourceInfo != null && parsedPublicationSourceInfo.length >= 2) {
            return parsedPublicationSourceInfo[1].trim();
        }

        return null;
    }

    /**
     * Parse publication source info value from document.
     *
     * @param document document to be parsed
     *
     * @return String[]
     */
    private static String[] parsePublicationSourceInfo(final Document document) {
        String sourceIdAndFormType = getFirstValueFromElement(document, "div.mainHeader");

        return sourceIdAndFormType != null ? sourceIdAndFormType.split("-") : null;
    }
}
