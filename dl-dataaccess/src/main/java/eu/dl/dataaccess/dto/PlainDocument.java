package eu.dl.dataaccess.dto;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;

/**
 * Class that holds plain document data. It is used as the connecting link between document saved with
 * {@link StorageService} and documents ({@link ParsedDocument}, {@link CleanDocument}, ...) objects.
 *
 * @author Tomas Mrazek
 */
@Entity
@Table(name = "plain_document")
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
     * File storage id ({@link StorageService#save(java.io.InputStream)}).
     */
    private String storageId;

    /**
     * File extension.
     */
    private String extension;

    /**
     * @return plain text
     */
    @Transient
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
    @Transient
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
    @Transient
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
    @Transient
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
    @Transient
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
    @Transient
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
    @Transient
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
}
