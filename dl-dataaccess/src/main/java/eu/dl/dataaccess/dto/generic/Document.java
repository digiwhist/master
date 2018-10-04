package eu.dl.dataaccess.dto.generic;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.PlainDocument;
import eu.dl.dataaccess.dto.clean.Validable;
import eu.dl.dataaccess.dto.codetables.DocumentType;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.dataaccess.utils.ValidationUtils;

import java.net.URL;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Metadata on document.
 */
@Transformable
public class Document implements MasterablePart, Validable {

    /**
     * Document title.
     */
    private String title;

    /**
     * Document type (agreement, tender etc...).
     */
    private DocumentType type;

    /**
     * Document URL.
     */
    private URL url;

    /**
     * Document publication date.
     */
    private LocalDateTime publicationDateTime;

    /**
     * Document signature date .
     */
    private LocalDate signatureDate;

    /**
     * Other versions of document (the actual document is recognized by highest
     * publicationDateTime).
     */
    private List<Document> otherVersions;

    /**
     * Appendices, amendments.
     */
    private List<Document> extensions;

    /**
     * Document version.
     */
    private String version;

    /**
     * Number of appendix, ammendment, version of document.
     */
    private Integer order;

    /**
     * Document description.
     */
    private String description;

    /**
     * File format (pdf, xls, doc, ...).
     */
    private String format;

    /**
     * Language ISO 639 code of the document.
     */
    private String language;

    /**
     * Plain document id ({@link PlainDocument}).
     */
    private String plainDocumentId;

    /**
     * Plain document ({@link PlainDocument}).
     */
    private PlainDocument plainDocument;

    /**
     * Tender id.
     */
    private String tenderId;

    /**
     * Publication date.
     */
    private LocalDate publicationDate;

    /**
     * @return the title
     */
    public final String getTitle() {
        return title;
    }

    /**
     * @param title
     *            the title to set
     * @return this instance for chaining
     */
    public final Document setTitle(final String title) {
        this.title = title;
        return this;
    }

    /**
     * @return the type
     */
    public final DocumentType getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     * @return this instance for chaining
     */
    public final Document setType(final DocumentType type) {
        this.type = type;
        return this;
    }

    /**
     * @return the url
     */
    public final URL getUrl() {
        return url;
    }

    /**
     * @param url
     *            the url to set
     * @return this instance for chaining
     */
    public final Document setUrl(final URL url) {
        this.url = url;
        return this;
    }

    /**
     * @return the publicationDateTime
     */
    public final LocalDateTime getPublicationDateTime() {
        return publicationDateTime;
    }

    /**
     * @param publicationDateTime
     *            the publicationDateTime to set
     * @return this instance for chaining
     */
    public final Document setPublicationDateTime(final LocalDateTime publicationDateTime) {
        this.publicationDateTime = publicationDateTime;
        return this;
    }

    /**
     * @return the signatureDate
     */
    public final LocalDate getSignatureDate() {
        return signatureDate;
    }

    /**
     * @param signatureDate
     *            the signatureDate to set
     * @return this instance for chaining
     */
    public final Document setSignatureDate(final LocalDate signatureDate) {
        this.signatureDate = signatureDate;
        return this;
    }

    /**
     * @return the otherVersions
     */
    public final List<Document> getOtherVersions() {
        return otherVersions;
    }

    /**
     * @param otherVersions
     *            the otherVersions to set
     * @return this instance for chaining
     */
    public final Document setOtherVersions(final List<Document> otherVersions) {
        this.otherVersions = otherVersions;
        return this;
    }

    /**
     * @return the extensions
     */
    public final List<Document> getExtensions() {
        return extensions;
    }

    /**
     * @param extensions
     *            the extensions to set
     * @return this instance for chaining
     */
    public final Document setExtensions(final List<Document> extensions) {
        this.extensions = extensions;
        return this;
    }

    /**
     * @return the version
     */
    public final String getVersion() {
        return version;
    }

    /**
     * @param version
     *            the version to set
     * @return this instance for chaining
     */
    public final Document setVersion(final String version) {
        this.version = version;
        return this;
    }

    /**
     * @return the order
     */
    public final Integer getOrder() {
        return order;
    }

    /**
     * @param order
     *            the order to set
     * @return this instance for chaining
     */
    public final Document setOrder(final Integer order) {
        this.order = order;
        return this;
    }

    /**
     * @return the description
     */
    public final String getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     * @return this instance for chaining
     */
    public final Document setDescription(final String description) {
        this.description = description;
        return this;
    }

    /**
     * @return the format
     */
    public final String getFormat() {
        return format;
    }

    /**
     * @param format
     *            the format to set
     * @return this instance for chaining
     */
    public final Document setFormat(final String format) {
        this.format = format;
        return this;
    }

    /**
     * @return the language
     */
    public final String getLanguage() {
        return language;
    }

    /**
     * @param language
     *            the language to set
     * @return this instance for chaining
     */
    public final Document setLanguage(final String language) {
        this.language = language;
        return this;
    }

    /**
     * @return plain document id
     */
    public final String getPlainDocumentId() {
        return plainDocumentId;
    }

    /**
     * @param plainDocumentId
     *            plain document id to be set
     * @return the basic parsed document
     */
    public final Document setPlainDocumentId(final String plainDocumentId) {
        this.plainDocumentId = plainDocumentId;
        return this;
    }

    /**
     * @return plain document
     */
    public final PlainDocument getPlainDocument() {
        return plainDocument;
    }

    /**
     * @param plainDocument
     *            plain document to be set
     * @return the basic parsed document
     */
    public final Document setPlainDocument(final PlainDocument plainDocument) {
        this.plainDocument = plainDocument;
        return this;
    }

    @Override
    public final String getTenderId() {
        return null;
    }

    /**
     * @param tenderId
     *            the tender id to set
     * @return this instance for chaining
     */
    public final Document setTenderId(final String tenderId) {
        this.tenderId = tenderId;
        return this;
    }

    @Override
    public final LocalDate getPublicationDate() {
        return publicationDate;
    }

    /**
     * @param publicationDate
     *            the tender id to set
     * @return this instance for chaining
     */
    public final Document setPublicationDate(final LocalDate publicationDate) {
        this.publicationDate = publicationDate;
        return this;
    }

    @Override
    @JsonIgnore
    public final Document getValid() {
        setExtensions(ValidationUtils.getValid(extensions));
        setOtherVersions(ValidationUtils.getValid(otherVersions));
        
        return ValidationUtils.getValid(this, description, extensions, format, language, order, otherVersions,
            plainDocumentId, publicationDate, publicationDateTime, signatureDate, tenderId, title, type, url, version);
    }

    @Override
    public final LocalDateTime getCreatedRaw() {
        return null;
    }
}
