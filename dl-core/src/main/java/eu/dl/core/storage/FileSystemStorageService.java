package eu.dl.core.storage;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.UUID;

import org.apache.commons.codec.digest.DigestUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.core.RecoverableException;
import eu.dl.core.UnrecoverableException;
import eu.dl.core.config.Config;

/**
 * Provides functionality related to store and handle file content to
 * filesystem.
 *
 */
public class FileSystemStorageService implements StorageService {
    
    private Logger logger;

    private String root;

    private static final String DEFAULT_NAMESPACE = "default";

    private static final String NAMESPACE_SEPARATOR = "::";

    /**
     * Creates storage service.
     * 
     */
    public FileSystemStorageService() {
        super();
        Config config = Config.getInstance();
        logger = LoggerFactory.getLogger(this.getClass());
        root = config.getParam("filesystemstorageservice.path");
        if (!Files.isDirectory(Paths.get(root))) {
            throw new UnrecoverableException(
                    String.format("Unable to initialise FileSystemStorageService. Directory %s does not exists", root));
        }

        // create dir for default namespace
        createDirIfNotExists(root + File.separator + DEFAULT_NAMESPACE);
    }

    /* (non-Javadoc)
     * @see eu.dl.core.storage.StorageService#save(java.io.InputStream)
     */
    @Override
    public final String save(final InputStream inputStream, final String namespace) {
        String uuid = UUID.randomUUID().toString();
        String sanitizedNamespace = DEFAULT_NAMESPACE;

        if (!namespace.equalsIgnoreCase(DEFAULT_NAMESPACE)) {
            sanitizedNamespace = DigestUtils.md5Hex(namespace);
        }

        String dirPath = getPathFromKey(sanitizedNamespace + NAMESPACE_SEPARATOR + uuid, true);

        try {
            Files.copy(inputStream, Paths.get(dirPath));
        } catch (IOException e) {
            logger.error("Unable to save file {}", e);
            throw new RecoverableException("Unable to save file", e);
        }

        logger.debug("File content stored into namespace {} with key {}", namespace, sanitizedNamespace + "::" + uuid);
        return sanitizedNamespace + "::" + uuid;
    }

    @Override
    public final String save(final InputStream inputStream) {
        return save(inputStream, DEFAULT_NAMESPACE);
    }

    /* (non-Javadoc)
     * @see eu.dl.core.storage.StorageService#get(java.lang.String)
     */
    @Override
    public final InputStream get(final String key) {
        validKey(key);

        String path = getPathFromKey(key, false);
        try {
            logger.debug("Returning file with key {}", key);
            return new FileInputStream(path);
        } catch (FileNotFoundException e) {
            logger.debug("File with key {} not found", key);
            return null;
        }
    }

    /**
     * @param key
     *      the key representing this file
     * @throws IllegalArgumentException
     *      in case that the given key is null or is invalid
     */
    private void validKey(final String key) throws IllegalArgumentException {
        if (key == null
            || !key.matches("[0-9a-f].*::[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}")) {
            throw new IllegalArgumentException(String.format("The key %s is not valid.", key));
        }
    }

    /**
     * Transforms key to path on the filesystem.
     * 
     * @param key
     *            the key to be transformed to filesystem path
     * @param create
     *            create directories when constructing the path
     * 
     * @return path on file system
     */
    private String getPathFromKey(final String key, final Boolean create) {
        String dirPath = root;

        // get namespace from key
        String namespace = key.split("::")[0];
        String uuid = key.split("::")[1];

        dirPath += File.separator + namespace;
        if (create) {
            createDirIfNotExists(dirPath);
        }

        for (int i = 0; i < 7; i += 2) {
            dirPath += File.separator + uuid.substring(i, i + 2);
            if (create) {
                createDirIfNotExists(dirPath);
            }
        }
        logger.debug("Key {} transformed to path {}", key, dirPath + File.separator + uuid);
        return dirPath + File.separator + uuid;
    }

    /**
     * Creates directory at path if does not exist.
     * 
     * @param path
     *            directory path
     * 
     */
    private void createDirIfNotExists(final String path) {
        File directory = new File(String.valueOf(path));
        if (!directory.exists()) {
            directory.mkdir();
        }
    }

    @Override
    public final Long getFileSize(final String key) {
        validKey(key);

        return (new File(getPathFromKey(key, false))).length();
    }

    @Override
    public final Boolean remove(final String key) {
        validKey(key);

        String path = getPathFromKey(key, false);
        try {
            File file = new File(path);
            if (file.exists()) {
                logger.debug("File with key '{}' deleted", key);
                return file.delete();
            } else {
                logger.debug("File with key '{}' is already deleted", key);
                return true;
            }
        } catch (SecurityException e) {
            logger.debug("Unable to delete file with key '{}' because of", key, e);
            return false;
        }
    }
}
