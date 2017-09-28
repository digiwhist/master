package eu.dl.dataaccess.dao;

/**
 * Basic dao methods.
 * 
 * @param <T>
 */
public interface BaseDAO<T> {
    /**
     * Set source name to be used for metadata info.
     *
     * @param sourceName
     *            name of the source(worker for example)
     */
    void setWorkerName(String sourceName);

    /**
     * Set source version to be used in metadata.
     *
     * @param sourceVersion
     *            version of the source(worker for example)
     */
    void setWorkerVersion(String sourceVersion);
}
