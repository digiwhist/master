package eu.dl.dataaccess.dto.ocds;


import eu.dl.dataaccess.annotation.Transformable;

/**
 * OCDS publisher. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSPublisher {

    private String name;

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
    public final OCDSPublisher setName(final String name) {
        this.name = name;
        return this;
    }
}
