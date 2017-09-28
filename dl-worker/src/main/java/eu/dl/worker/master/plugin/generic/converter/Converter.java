package eu.dl.worker.master.plugin.generic.converter;

/**
 * This utility class is used to convert objects from matched to master if needed.
 */
public interface Converter {
    /**
     * Converts object in case of need. For example from MatchedBody to MasterBody etc.
     * @param object
     *      list of objects to be converted
     * @return
     *      converted object
     */
    Object convert(Object object);
}
