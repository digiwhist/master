package eu.dl.core.storage;

/**
 * Factory for storage services.
 */
public class StorageServiceFactory {

    /**
     * Default constructor.
     */
    protected StorageServiceFactory() {
        // no public constructor available
    }

    /**
     * Creates and returns storage service.
     * 
     * @return storage service
     */
    public static final StorageService getStorageService() {
        return new FileSystemStorageService();
    }
}
