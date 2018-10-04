package eu.dl.dataaccess.dto.parsed;

// TODO: Auto-generated Javadoc
/**
 * Unit price.
 */
public class ParsedUnitPrice extends BaseParsedPrice<ParsedUnitPrice> {

    /**
     * Number of units.
     */
    private String unitNumber;

    /**
     * Unit of price given if it is unit price (e.g. EUR/KG).
     */
    private String unitType;

    /**
     * Description of unit.
     */
    private String description;

    /**
     * Gets the unit number.
     *
     * @return the unit number
     */
    public final String getUnitNumber() {
        return unitNumber;
    }

    /**
     * Sets the unit number.
     *
     * @param unitNumber
     *            the unit number
     * @return the parsed unit price
     */
    public final ParsedUnitPrice setUnitNumber(final String unitNumber) {
        this.unitNumber = unitNumber;
        return this;
    }

    /**
     * Gets the unit type.
     *
     * @return the unit type
     */
    public final String getUnitType() {
        return unitType;
    }

    /**
     * Sets the unit type.
     *
     * @param unitType
     *            the unit type
     * @return the parsed unit price
     */
    public final ParsedUnitPrice setUnitType(final String unitType) {
        this.unitType = unitType;
        return this;
    }

    /**
     * @return unit description
     */
    public final String getDescription() {
        return description;
    }

    /**
     * @param description
     *      unit description to be set
     * @return this instance for chaining
     */
    public final ParsedUnitPrice setDescription(final String description) {
        this.description = description;
        return this;
    }
}
