package eu.dl.dataaccess.dto.ocds;


import eu.dl.dataaccess.annotation.Transformable;

/**
 * OCDS lot extension. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/extensions/lots/">Lots Extension</a>
 */
@Transformable
public class OCDSLot extends BaseOCDSExtension {
    /**
     * LOT Extension URL.
     */
    public static final String EXTENSION_URL =
        "https://raw.githubusercontent.com/open-contracting/ocds_lots_extension/v1.1.1/extension.json";

    private String id;

    private String title;

    private String description;

    private String status;

    private OCDSValue value;

    @Override
    public final String getUrl() {
        return EXTENSION_URL;
    }

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
    public final OCDSLot setId(final String id) {
        this.id = id;
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
    public final OCDSLot setTitle(final String title) {
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
    public final OCDSLot setDescription(final String description) {
        this.description = description;
        return this;
    }

    /**
     * @return status
     */
    public final String getStatus() {
        return status;
    }

    /**
     * @param status
     *      status to be set
     * @return this instance for chaining
     */
    public final OCDSLot setStatus(final String status) {
        this.status = status;
        return this;
    }

    /**
     * @return value
     */
    public final OCDSValue getValue() {
        return value;
    }

    /**
     * @param value
     *      value to be set
     * @return this instance for chaining
     */
    public final OCDSLot setValue(final OCDSValue value) {
        this.value = value;
        return this;
    }
}
