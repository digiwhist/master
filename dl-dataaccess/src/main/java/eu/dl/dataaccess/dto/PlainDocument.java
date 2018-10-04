package eu.dl.dataaccess.dto;

import java.util.Map;

/**
 * Class that holds plain document data. It is used as the connecting link between document saved with
 * {@link eu.dl.core.storage.StorageService} and documents ({@link eu.dl.dataaccess.dto.parsed.ParsedDocument},
 * {@link eu.dl.dataaccess.dto.generic.Document}, ...) objects.
 *
 * @author Tomas Mrazek
 */
public class PlainDocument extends StorableDTO {
    /**
     * Plain text of text files.
     */
    private String plainText;

    /**
     * MIME type.
     */
    private String contentType;

    /**
     * File size in bytes.
     */
    private Long contentLength;

    /**
     * Name.
     */
    private String name;

    /**
     * This value is used for file identification. Usually, it is a hash from data that aren't saved in PlainDocument.
     */
    private String hash;

    /**
     * File storage id ({@link eu.dl.core.storage.StorageService#save(java.io.InputStream)}).
     */
    private String storageId;

    /**
     * File extension.
     */
    private String extension;

    /**
     * List of file response headers.
     */
    private Map<String, String> rawHeaders;

    /**
     * @return plain text
     */
    public final String getPlainText() {
        return plainText;
    }

    /**
     * @param plainText
     *      plain text to be set
     * @return this instance for chaining
     */
    public final PlainDocument setPlainText(final String plainText) {
        this.plainText = plainText;
        return this;
    }

    /**
     * @return file content type
     */
    
    public final String getContentType() {
        return contentType;
    }

    /**
     * @param contentType
     *      content type to be set
     * @return this instance for chaining
     */
    public final PlainDocument setContentType(final String contentType) {
        this.contentType = contentType;
        return this;
    }

    /**
     * @return file content length
     */
    
    public final Long getContentLength() {
        return contentLength;
    }

    /**
     *
     * @param contentLength
     *      file content length to be set
     * @return this instance for chaining
     */
    public final PlainDocument setContentLength(final Long contentLength) {
        this.contentLength = contentLength;
        return this;
    }

    /**
     * @return name
     */
    
    public final String getName() {
        return name;
    }

    /**
     * @param name
     *      name to be set
     * @return this instance for chaining
     */
    public final PlainDocument setName(final String name) {
        this.name = name;
        return this;
    }

    /**
     * @return file storage id
     */
    
    public final String getStorageId() {
        return storageId;
    }

    /**
     * @param storageId
     *      file storage id to be set
     * @return this instance for chaining
     */
    public final PlainDocument setStorageId(final String storageId) {
        this.storageId = storageId;
        return this;
    }

    /**
     * @return hash
     */
    
    public final String getHash() {
        return hash;
    }

    /**
     * @param hash
     *      hash to be set
     * @return this instance for chaining
     */
    public final PlainDocument setHash(final String hash) {
        this.hash = hash;
        return this;
    }

    /**
     * @return file extension
     */
    
    public final String getExtension() {
        return extension;
    }

    /**
     * @param extension
     *      file extension to be set
     * @return this instance for chaining
     */
    public final PlainDocument setExtension(final String extension) {
        this.extension = extension;
        return this;
    }

    /**
     * @return list of response headers
     */
    public final Map<String, String> getRawHeaders() {
        return rawHeaders;
    }

    /**
     * @param rawHeaders
     *      header to be set
     * @return this instance for chaining
     */
    public final PlainDocument setRawHeaders(final Map<String, String> rawHeaders) {
        this.rawHeaders = rawHeaders;
        return this;
    }
}
