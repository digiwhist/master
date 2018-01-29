package eu.dl.dataaccess.dto.ocds;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.codetables.OCDSDocumentType;
import java.net.URL;
import java.time.LocalDateTime;

/**
 * OCDS document. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSDocument {

    private String id;
    /**
     * tender/documents/type or tender/publications/formType mapped to OCDS codelist.
     */
    private OCDSDocumentType type;

    private String title;

    private String description;

    private URL url;

    private LocalDateTime published;

    private LocalDateTime modified;

    private String format;

    /**
     *  either two-digit ISO 639-1, or extended BCP47 language tags.
     */
    private String language;

    /**
     * @return id
     */
    public final String getId() {
        return id;
    }

    /**
     * @param id
     *      id to be set
     * @return this instance for chaining
     */
    public final OCDSDocument setId(final String id) {
        this.id = id;
        return this;
    }

    /**
     * @return type
     */
    @JsonProperty("documentType")
    public final OCDSDocumentType getType() {
        return type;
    }

    /**
     * @param type
     *      type to be set
     * @return this instance for chaining
     */
    public final OCDSDocument setType(final OCDSDocumentType type) {
        this.type = type;
        return this;
    }

    /**
     * @return title
     */
    public final String getTitle() {
        return title;
    }

    /**
     * @param title
     *      title to be set
     * @return this instance for chaining
     */
    public final OCDSDocument setTitle(final String title) {
        this.title = title;
        return this;
    }

    /**
     * @return description
     */
    public final String getDescription() {
        return description;
    }

    /**
     * @param description
     *      description to be set
     * @return this instance for chaining
     */
    public final OCDSDocument setDescription(final String description) {
        this.description = description;
        return this;
    }

    /**
     * @return url
     */
    public final URL getUrl() {
        return url;
    }

    /**
     * @param url
     *      url to be set
     * @return this instance for chaining
     */
    public final OCDSDocument setUrl(final URL url) {
        this.url = url;
        return this;
    }

    /**
     * @return publish date
     */
    @JsonProperty("datePublished")
    @JsonSerialize(using = OCDSLocalDateTimeSerializer.class)
    public final LocalDateTime getPublished() {
        return published;
    }

    /**
     * @param published
     *      publish date to be set
     * @return this instance for chaining
     */
    public final OCDSDocument setPublished(final LocalDateTime published) {
        this.published = published;
        return this;
    }

    /**
     * @return modification date
     */
    @JsonProperty("dateModified")
    @JsonSerialize(using = OCDSLocalDateTimeSerializer.class)
    public final LocalDateTime getModified() {
        return modified;
    }

    /**
     * @param modified
     *      modification date to be set
     * @return this instance for chaining
     */
    public final OCDSDocument setModified(final LocalDateTime modified) {
        this.modified = modified;
        return this;
    }

    /**
     * @return format
     */
    public final String getFormat() {
        return format;
    }

    /**
     * @param format
     *      format to be set
     * @return this instance for chaining
     */
    public final OCDSDocument setFormat(final String format) {
        this.format = format;
        return this;
    }

    /**
     * @return language
     */
    @JsonSerialize(using = OCDSLanguageSerializer.class)
    public final String getLanguage() {
        return language;
    }

    /**
     * @param language
     *      language to be set
     * @return this instance for chaining
     */
    public final OCDSDocument setLanguage(final String language) {
        this.language = language;
        return this;
    }
}
