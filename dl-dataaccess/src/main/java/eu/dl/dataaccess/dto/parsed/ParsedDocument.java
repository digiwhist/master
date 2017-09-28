package eu.dl.dataaccess.dto.parsed;

import java.util.List;

// TODO: Auto-generated Javadoc
/**
 * Metadata on document.
 */
public class ParsedDocument {

    /**
     * Document title.
     */
    private String title;

    /**
     * Document type (agreement, tender etc...).
     */
    private String type;

    /**
     * Document URL.
     */
    private String url;

    /**
     * Document publication date.
     */
    private String publicationDateTime;

    /**
     * Document signature date .
     */
    private String signatureDate;

    /**
     * Other versions of document (the actual document is recognized by highest
     * publicationDateTime).
     */
    private List<ParsedDocument> otherVersions;

    /**
     * Appendices, amendments.
     */
    private List<ParsedDocument> extensions;

    /**
     * Document version.
     */
    private String version;

    /**
     * Number of appendix, ammendment, version of document.
     */
    private String order;

    /**
     * Document description.
     */
    private String description;

    /**
     * File format (pdf, xls, doc, ...).
     */
    private String format;

    /**
     * Language code of the document.
     */
    private String language;

    /**
     * Plain document id ({@link PlainDocument}).
     */
    private String plainDocumentId;

    /**
     * Gets the title.
     *
     * @return the title
     */
    public final String getTitle() {
        return title;
    }

    /**
     * Sets the title.
     *
     * @param title
     *            the title
     * @return the basic parsed document
     */
    public final ParsedDocument setTitle(final String title) {
        this.title = title;
        return this;
    }

    /**
     * Gets the type.
     *
     * @return the type
     */
    public final String getType() {
        return type;
    }

    /**
     * Sets the type.
     *
     * @param type
     *            the type
     * @return the basic parsed document
     */
    public final ParsedDocument setType(final String type) {
        this.type = type;
        return this;
    }

    /**
     * Gets the url.
     *
     * @return the url
     */
    public final String getUrl() {
        return url;
    }

    /**
     * Sets the url.
     *
     * @param url
     *            the url
     * @return the basic parsed document
     */
    public final ParsedDocument setUrl(final String url) {
        this.url = url;
        return this;
    }

    /**
     * Gets the publication date time.
     *
     * @return the publication date time
     */
    public final String getPublicationDateTime() {
        return publicationDateTime;
    }

    /**
     * Sets the publication date time.
     *
     * @param publicationDateTime
     *            the publication date time
     * @return the basic parsed document
     */
    public final ParsedDocument setPublicationDateTime(final String publicationDateTime) {
        this.publicationDateTime = publicationDateTime;
        return this;
    }

    /**
     * Gets the signature date.
     *
     * @return the signature date
     */
    public final String getSignatureDate() {
        return signatureDate;
    }

    /**
     * Sets the signature date.
     *
     * @param signatureDate
     *            the signature date
     * @return the basic parsed document
     */
    public final ParsedDocument setSignatureDate(final String signatureDate) {
        this.signatureDate = signatureDate;
        return this;
    }

    /**
     * Gets the other versions.
     *
     * @return the other versions
     */
    public final List<ParsedDocument> getOtherVersions() {
        return otherVersions;
    }

    /**
     * Sets the other versions.
     *
     * @param otherVersions
     *            the other versions
     * @return the basic parsed document
     */
    public final ParsedDocument setOtherVersions(final List<ParsedDocument> otherVersions) {
        this.otherVersions = otherVersions;
        return this;
    }

    /**
     * Gets the extensions.
     *
     * @return the extensions
     */
    public final List<ParsedDocument> getExtensions() {
        return extensions;
    }

    /**
     * Sets the extensions.
     *
     * @param extensions
     *            the extensions
     * @return the basic parsed document
     */
    public final ParsedDocument setExtensions(final List<ParsedDocument> extensions) {
        this.extensions = extensions;
        return this;
    }

    /**
     * Gets the version.
     *
     * @return the version
     */
    public final String getVersion() {
        return version;
    }

    /**
     * Sets the version.
     *
     * @param version
     *            the version
     * @return the basic parsed document
     */
    public final ParsedDocument setVersion(final String version) {
        this.version = version;
        return this;
    }

    /**
     * Gets the order.
     *
     * @return the order
     */
    public final String getOrder() {
        return order;
    }

    /**
     * Sets the order.
     *
     * @param order
     *            the order
     * @return the basic parsed document
     */
    public final ParsedDocument setOrder(final String order) {
        this.order = order;
        return this;
    }

    /**
     * Gets the description.
     *
     * @return the description
     */
    public final String getDescription() {
        return description;
    }

    /**
     * Sets the description.
     *
     * @param description
     *            the description
     * @return the basic parsed document
     */
    public final ParsedDocument setDescription(final String description) {
        this.description = description;
        return this;
    }

    /**
     * Gets the format.
     *
     * @return the format
     */
    public final String getFormat() {
        return format;
    }

    /**
     * Sets the format.
     *
     * @param format
     *            the format
     * @return the basic parsed document
     */
    public final ParsedDocument setFormat(final String format) {
        this.format = format;
        return this;
    }

    /**
     * Gets the language.
     *
     * @return the language
     */
    public final String getLanguage() {
        return language;
    }

    /**
     * Sets the language.
     *
     * @param language
     *            the language
     * @return the basic parsed document
     */
    public final ParsedDocument setLanguage(final String language) {
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
    public final ParsedDocument setPlainDocumentId(final String plainDocumentId) {
        this.plainDocumentId = plainDocumentId;
        return this;
    }
}
