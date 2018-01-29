package eu.dl.dataaccess.dto.ocds;


import eu.dl.dataaccess.annotation.Transformable;
import java.util.ArrayList;
import java.util.List;

/**
 * Base class for OCDS oraganization entities. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 *
 * @param <T>
 *      underlying class
 */
@Transformable
public abstract class BaseOCDSOrganization<T> {

    private String id;

    private String name;

    private OCDSIdentifier identifier;

    private List<OCDSIdentifier> additionalIdentifiers;

    private OCDSAddress address;

    private OCDSContactPoint contactPoint;

    /**
     * @return id
     */
    public final String getId() {
        return id;
    }

    /**
     * @param newId
     *      id to be set
     * @return this instance for chaining
     */
    public final T setId(final String newId) {
        this.id = newId;
        return (T) this;
    }

    /**
     * @return name
     */
    public final String getName() {
        return name;
    }

    /**
     * @param newName
     *      name to be set
     * @return this instance for chaining
     */
    public final T setName(final String newName) {
        this.name = newName;
        return (T) this;
    }

    /**
     * @return identifier
     */
    public final OCDSIdentifier getIdentifier() {
        return identifier;
    }

    /**
     * @param newIdentifier
     *      identifier to be set
     * @return this instance for chaining
     */
    public final T setIdentifier(final OCDSIdentifier newIdentifier) {
        this.identifier = newIdentifier;
        return (T) this;
    }

    /**
     * @return address
     */
    public final OCDSAddress getAddress() {
        return address;
    }

    /**
     * @param newAddress
     *      address to be set
     * @return this instance for chaining
     */
    public final T setAddress(final OCDSAddress newAddress) {
        this.address = newAddress;
        return (T) this;
    }

    /**
     * @return contact point
     */
    public final OCDSContactPoint getContactPoint() {
        return contactPoint;
    }

    /**
     * @param newContactPoint
     *      contact point to be set
     * @return this instance for chaining
     */
    public final T setContactPoint(final OCDSContactPoint newContactPoint) {
        this.contactPoint = newContactPoint;
        return (T) this;
    }

    /**
     * @return list of additional identifiers
     */
    public final List<OCDSIdentifier> getAdditionalIdentifiers() {
        return additionalIdentifiers;
    }

    /**
     * @param newAdditionalIdentifiers
     *      list of additional identifiers to be set
     * @return this instance for chaining
     */
    public final T setAdditionalIdentifiers(final List<OCDSIdentifier> newAdditionalIdentifiers) {
        this.additionalIdentifiers = newAdditionalIdentifiers;
        return (T) this;
    }

    /**
     * Adds additonal identifier. List is created if needed.
     *
     * @param additonalIdentifier
     *      additonal identifier to be added
     * @return this instance for chaining
     */    
    public final T addAdditionalIdentifier(final OCDSIdentifier additonalIdentifier) {
        if (additonalIdentifier != null) {
            if (this.additionalIdentifiers == null) {
                this.additionalIdentifiers = new ArrayList<>();
            }

            this.additionalIdentifiers.add(additonalIdentifier);
        }

        return (T) this;
    }
}
