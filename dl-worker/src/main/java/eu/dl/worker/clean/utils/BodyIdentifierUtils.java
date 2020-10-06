package eu.dl.worker.clean.utils;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier.Scope;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier.Type;

import java.util.Arrays;
import java.util.List;

/**
 * This class provides method for body identifier cleaning.
 *
 * @author Marek Mikes
 */
public final class BodyIdentifierUtils {

    /**
     * List of scopes to ignore identifier length for.
     */
    private static final List<Scope> COUNTRIES_TO_IGNORE_IDENTIFIER_LENGTH = Arrays.asList(Scope.UG);

    /**
     * Utility classes should not have default constructor.
     */
    private BodyIdentifierUtils() {

    }

    /**
     * Cleans the given parsed body identifier.
     *
     * @param bodyIdentifier
     *         parsed body identifier
     *
     * @return cleaned body identifier
     */
    public static BodyIdentifier cleanBodyIdentifier(final BodyIdentifier bodyIdentifier) {
        if (bodyIdentifier == null) {
            return null;
        }

        return new BodyIdentifier()
                .setId(StringUtils.cleanBodyIdentifier(bodyIdentifier.getId(), ignoreIdentifierLength(bodyIdentifier.getScope())))
                .setType(bodyIdentifier.getType() == null ? Type.ORGANIZATION_ID : bodyIdentifier.getType())
                .setScope(bodyIdentifier.getScope() == null ? Scope.UNKNOWN : bodyIdentifier.getScope());
    }

    /**
     * Defines if identifier length should be ignored for given scope.
     * @param scope scope
     * @return true if identifier length should be ignored, false otherwise
     */
    private static boolean ignoreIdentifierLength(final Scope scope) {
        if(scope == null) {
            return false;
        }
        return COUNTRIES_TO_IGNORE_IDENTIFIER_LENGTH.contains(scope);
    }

}
