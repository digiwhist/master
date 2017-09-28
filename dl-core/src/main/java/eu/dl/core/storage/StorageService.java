package eu.dl.core.storage;

import java.io.InputStream;

/**
 * Provides functionality related to store and handle file content.
 *
 */
public interface StorageService {

    /**
     * Saves inputstream content to persistent storage and returns a key. The
     * data are stored into assigned namespace. The returned key is unique
     * accross the namespaces.
     * 
     * @param inputStream
     *            the inputStream to be stored
     * @param namespace
     *            the namespace where to store the file
     * @return key assigned to this file
     */
    String save(InputStream inputStream, String namespace);

    /**
     * Saves inputstream content to persistent storage and returns a key. The
     * data are stored into default namespace. The returned key is unique
     * across the namespaces.
     * 
     * @param inputStream
     *            the inputStream to be stored
     * @return key assigned to this file
     */
    String save(InputStream inputStream);

    /**
     * Gets the file from persistent storage.
     * 
     * @param key
     *            the key representing this file
     * 
     * @return input stream of the file or null if the file is not found
     */
    InputStream get(String key);

    /**
     * Gets the file from persistent storage and returns its size in bytes.
     *
     * @param key
     *            the key representing this file
     *
     * @return size of file in bytes or null if the file doesn't exist
     */
    Long getFileSize(String key);

    /**
     * Removes the file from persistent storage.
     *
     * @param key
     *            the key representing this file
     *
     * @return true only and only if the file removing was successful
     */
    Boolean remove(String key);
}