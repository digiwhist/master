package eu.datlab.worker.matched;

import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedTender;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Useful methods for matching plugins.
 */
public final class MatchingPluginUtils {
    /**
     * Suppress default constructor.
     */
    private MatchingPluginUtils() {
    }

    /**
     * @param t
     *      matched tender
     * @return stream of tender's publications or empty stream
     */
    public static Stream<Publication> getPublicationsAsStream(final MatchedTender t) {
        return Optional.ofNullable(t.getPublications()).orElse(Collections.emptyList()).stream();
    }

    /**
     * @param t
     *      matched tender
     * @param getter
     *      function used for retrieving value
     * @param <T>
     *      type of retrieved values
     * @return distinct list of non-null values or empty list
     */
    public static <T> List<T> getPublicationsValues(final MatchedTender t, final Function<Publication, T> getter) {
        return getPublicationsAsStream(t)
            .map(getter)
            .filter(Objects::nonNull)
            .distinct()
            .collect(Collectors.toList());
    }
}
