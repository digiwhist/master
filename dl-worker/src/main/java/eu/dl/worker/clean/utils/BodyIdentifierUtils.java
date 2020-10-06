package eu.dl.worker.clean.utils;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier.Scope;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier.Type;

/**
 * This class provides method for body identifier cleaning.
 *
 * @author Marek Mikes
 */
public final class BodyIdentifierUtils {    
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
                .setId(StringUtils.cleanBodyIdentifier(bodyIdentifier.getId()))
                .setType(bodyIdentifier.getType() == null ? Type.ORGANIZATION_ID : bodyIdentifier.getType())
                .setScope(bodyIdentifier.getScope() == null ? Scope.UNKNOWN : bodyIdentifier.getScope());
    }
}
