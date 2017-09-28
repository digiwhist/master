package eu.dl.dataaccess.dto.clean;

/**
 * Shared interface for all classes that need to remove nonsenses.
 *
 * @author Tomas Mrazek
 */
public interface Validable {

    /**
     * @return null if the instance is empty, otherwise this instance without nonsenses
     */
    Validable getValid();
}
