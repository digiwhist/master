package eu.dl.dataaccess.dto.matched;

/**
 * Pool body used in exact and approximate matching plugin.
 *
 * @author Tomas Mrazek
 */
public interface PoolBody {
    /**
     * @return body id
     */
    String getId();

    /**
     * @return group id
     */
    String getGroupId();
}
