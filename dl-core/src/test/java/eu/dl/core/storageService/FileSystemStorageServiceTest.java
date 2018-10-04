package eu.dl.core.storageService;

import static eu.dl.core.ThrowableAssertion.assertThrown;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.LoggerFactory;

import eu.dl.core.config.Config;
import eu.dl.core.storage.FileSystemStorageService;

/**
 * Test of storage service.
 *
 */
public class FileSystemStorageServiceTest {

    private FileSystemStorageService storageService;

    /**
     * Initialisation of storage service used in test methods.
     */
    @Before
    public final void init() {
        Config.getInstance().addConfigFile("unit_test");
        Config config = Config.getInstance();
        String storageServicePath = config.getParam("filesystemstorageservice.path");
        
        // try to create folder
        try {
            File f = new File(storageServicePath);
            f.mkdir();
        } catch (Exception e) {
            LoggerFactory.getLogger("Unit test")
                    .warn("Unable to create filesystem storage path '{}' with excpetion {}", storageServicePath, e);
        }
        storageService = new FileSystemStorageService();
    }

    /**
     * Test of null value.
     */
    @Test
    public final void nullValueTest() {
        assertThrown(() -> storageService.get(null)).isInstanceOf(IllegalArgumentException.class);
    }

    /**
     * Test of null value.
     */
    @Test
    public final void invalidKeyFormatTest() {
        assertThrown(() -> storageService.get("invalid format")).isInstanceOf(IllegalArgumentException.class);
    }

    /**
     * Test of null value.
     */
    @Test
    public final void nonexistentKeyFormatTest() {
        assertNull(storageService.get("89801e9e98979062e84645633a8ed3e9::f8c0c37e-202e-4706-af17-486750c3f406"));
    }

    /**
     * @throws IOException
     *             in case of failure
     */
    @Test
    public final void saveAndReadTest() throws IOException {
        InputStream inputStream = new ByteArrayInputStream("test".getBytes());
        String key = storageService.save(inputStream);

        ByteArrayOutputStream result = new ByteArrayOutputStream();
        InputStream returnedStream = storageService.get(key);
        byte[] buffer = new byte[1024];
        int length;
        while ((length = returnedStream.read(buffer)) != -1) {
            result.write(buffer, 0, length);
        }

        assertTrue("test".equals(result.toString("UTF-8")));
    }

    /**
     * @throws IOException
     *             in case of failure
     */
    @Test
    public final void saveAndReadNamespaceTest() throws IOException {
        InputStream inputStream = new ByteArrayInputStream("test".getBytes());
        String key = storageService.save(inputStream, "namespace");

        ByteArrayOutputStream result = new ByteArrayOutputStream();
        InputStream returnedStream = storageService.get(key);
        byte[] buffer = new byte[1024];
        int length;
        while ((length = returnedStream.read(buffer)) != -1) {
            result.write(buffer, 0, length);
        }

        assertTrue("test".equals(result.toString("UTF-8")));
    }
}

