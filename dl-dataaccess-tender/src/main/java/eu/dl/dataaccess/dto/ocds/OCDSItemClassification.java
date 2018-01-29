package eu.dl.dataaccess.dto.ocds;


import eu.dl.dataaccess.annotation.Transformable;
import java.net.URI;

/**
 * OCDS item classification. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSItemClassification {

    private String id;

    private String scheme;

    private String description;
    
    private URI uri;

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
    public final OCDSItemClassification setId(final String id) {
        this.id = id;
        return this;
    }

    /**
     * @return scheme
     */
    public final String getScheme() {
        return scheme;
    }

    /**
     * @param scheme
     *      scheme to be set
     * @return this instance for chaining
     */
    public final OCDSItemClassification setScheme(final String scheme) {
        this.scheme = scheme;
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
    public final OCDSItemClassification setDescription(final String description) {
        this.description = description;
        return this;
    }

    /**
     * @return uri
     */
    public final URI getUri() {
        return uri;
    }

    /**
     * @param uri
     *      uri to be set
     * @return this instance for chaining
     */
    public final OCDSItemClassification setUri(final URI uri) {
        this.uri = uri;
        return this;
    }
}
