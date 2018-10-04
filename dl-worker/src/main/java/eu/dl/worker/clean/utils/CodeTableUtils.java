package eu.dl.worker.clean.utils;

import java.util.ArrayList;
import java.util.Collections;
import static org.apache.commons.lang3.StringUtils.getLevenshteinDistance;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class holds methods handy for code table handling.
 *
 * @author Kuba Krafka
 */
public final class CodeTableUtils {

    private static final Logger logger = LoggerFactory.getLogger(CodeTableUtils.class.getName());

    private static final Integer LEVENSHTEIN_THRESHOLD = 4;

    /**
     * Utility classes should not have default constructor.
     */
    private CodeTableUtils() {

    }

    /**
     * Maps the string value to enum value. Mapping should be provided in a from of
     * Enum.APPLE => ("apple","apples", "aple")
     * Enum.MAPPLE => ("mapple", "maplpe")
     * If there is direct match by equality, value is immediately returned. Otherwise,
     * Levenshtein distance is calculated for each key(lowest from all the possible keys)
     * and if there is one lowest distance found, the key is returned as a match.
     * If there are two keys with the same distance, the exception is thrown, as there
     * is no single candidate found.
     *
     * @param input
     *         value to be mapped
     * @param mapping
     *         mapping
     * @param defaultEnumValue
     *         the not null enum value that is returned if the match is not found
     * @param tryFuzzyMatch
     *         boolean whether fuzzy match should be tried when exact match is not found
     *
     * @return mapped value
     */
    public static Enum mapValue(final String input, final Map<Enum, List<String>> mapping, final Enum defaultEnumValue,
            final boolean tryFuzzyMatch) {
        logger.debug("Mapping value {}", input);

        final String inputForCleaning = StringUtils.prepareStringForCleaning(input);
        if (inputForCleaning == null || inputForCleaning.isEmpty() || mapping == null) {
            return null;
        } else if (mapping.isEmpty()) {
            return defaultEnumValue;
        }
        
        String enumClass;
        if (defaultEnumValue != null) {
            enumClass = defaultEnumValue.getClass().getName();
        } else {
            enumClass = mapping.keySet().stream()
                .filter(Objects::nonNull)
                .map(n -> n.getClass().getName())
                .findFirst()
                .orElse(null);
        }
        // try the simple match
        for (Map.Entry<Enum, List<String>> entry : mapping.entrySet()) {
            Enum key = entry.getKey();
            for (String value : entry.getValue()) {
                if (inputForCleaning.equalsIgnoreCase(value)) {
                    logger.debug("Value '{}' found, returning mapping result '{}'", value, key);
                    return key;
                }
            }
        }

        // try to find the value
        if (tryFuzzyMatch) {
            Enum bestKey = null;
            Integer bestLDistance = LEVENSHTEIN_THRESHOLD;
            Boolean isBestScoreKeySingleOne = true;
            for (Map.Entry<Enum, List<String>> entry : mapping.entrySet()) {
                Enum key = entry.getKey();
                Integer bestKeyLDistance = LEVENSHTEIN_THRESHOLD;
                for (String value : entry.getValue()) {
                    // try the simple match(if there is equality)
                    if (inputForCleaning.equalsIgnoreCase(value)) {
                        logger.debug("Value '{}' found, returning mapping result '{}'", value, key);
                        return key;
                    } else {
                        // get Levenshtein distance between strings
                        Integer lDistance = getLevenshteinDistance(inputForCleaning.toLowerCase(), value.toLowerCase());
                        if (lDistance < LEVENSHTEIN_THRESHOLD) {
                            // great, the distance is lower then threshold

                            // is it better than other ones for this key
                            if (lDistance < bestKeyLDistance) {
                                // great, its the local minimum so far
                                bestKeyLDistance = lDistance;

                                // check whether it equals to the global minimum
                                if (lDistance == bestLDistance) {
                                    // we have two keys with the same score,
                                    // thats bad for cleaning as we are unable to
                                    // choose the proper one
                                    isBestScoreKeySingleOne = false;
                                }

                                // check whether its better than global minimum
                                if (lDistance < bestLDistance) {
                                    // we have the new minimum
                                    bestKey = key;
                                    bestLDistance = lDistance;
                                    isBestScoreKeySingleOne = true;
                                }
                            }
                        }
                    }
                }
            }

            if (!isBestScoreKeySingleOne) {
                // unfortunately the Levenshtein distance for more keys is the same
                // there is no single candidate
                logger.error("Cleaning failed for {} - found more than one candidates with levenshtein = {}, "
                        + "unable to pick single one.",
                        enumClass, bestLDistance);
                return null;
            }

            // check the scores calculated by Levenshtein
            if (bestLDistance < LEVENSHTEIN_THRESHOLD && bestKey != null) {
                logger.debug("Key {} selected based on Levensthein distance of {}.", bestKey, bestLDistance);
                return bestKey;
            }
        }

        if (defaultEnumValue != null) {
            return defaultEnumValue;
        }

        logger.error("Cleaning failed for {} - unique value not found for {}, throwing an exception", enumClass,
            inputForCleaning);
        return null;
    }

    /**
     * Maps the string value to enum value (tries both - exact match and fuzzy match using levenshtein).
     *
     * @param input
     *         value to be mapped
     * @param mapping
     *         mapping
     *
     * @return mapped value
     * @see CodeTableUtils
     */
    public static Enum mapValue(final String input, final Map<Enum, List<String>> mapping) {
        return mapValue(input, mapping, null, true);
    }

    /**
     * Maps the string value to enum value (tries both - exact match and fuzzy match using levenshtein).
     *
     * @param input
     *         value to be mapped
     * @param mapping
     *         mapping
     * @param defaultEnumValue
     *         the not null enum value that is returned if the match is not found
     *
     * @return mapped value
     * @see CodeTableUtils
     */
    public static Enum mapValue(final String input, final Map<Enum, List<String>> mapping,
            final Enum defaultEnumValue) {
        return mapValue(input, mapping, defaultEnumValue, true);
    }

    /**
     * Creates mapping for the given {@code enumeration} where each item from enumeration is mappend on its name.
     *
     * @param <E>
     *         enumeration class
     * @param enumeration
     *         mapped enumeration
     *
     * @return mapping
     */
    public static <E extends Enum<E>> HashMap<Enum, List<String>> enumToMapping(final Class<E> enumeration) {
        return enumToMapping(enumeration, e -> e.name());
    }

    /**
     * Creates mapping for the given {@code enumeration} where each enumeration item is mappend on value as an output of
     * {@code mapper}.
     *
     * @param <E>
     *         enumeration class
     * @param enumeration
     *         mapped enumeration
     * @param mapper
     *         function that accepts enumeration item as input parameter and returns string
     *
     * @return mapping
     */
    public static <E extends Enum<E>> HashMap<Enum, List<String>> enumToMapping(final Class<E> enumeration,
            final Function<Enum, String> mapper) {
        HashMap<Enum, List<String>> mapping = new HashMap<>();

        for (Object item : enumeration.getEnumConstants()) {
            Enum enumItem = (Enum) item;
            mapping.put(enumItem, new ArrayList<>(Collections.singletonList(mapper.apply(enumItem))));
        }

        return mapping;
    }
}
