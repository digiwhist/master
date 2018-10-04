package eu.dl.dataaccess.dto.generic;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.clean.Validable;
import eu.dl.dataaccess.dto.codetables.UnitType;
import eu.dl.dataaccess.utils.ValidationUtils;

/**
 * Unit price.
 */
@Transformable
public class UnitPrice extends BasePrice<UnitPrice> implements Validable {

    /**
     * Number of units.
     */
    private Integer unitNumber;

    /**
     * Unit of price given if it is unit price (e.g. EUR/KG).
     */
    private UnitType unitType;

    /**
     * Description of unit.
     */
    private String description;

    /**
     * @return number of units
     */
    public final Integer getUnitNumber() {
        return unitNumber;
    }

    /**
     * @param unitNumber
     *            number of units
     * @return this instance for chaining
     */
    public final UnitPrice setUnitNumber(final Integer unitNumber) {
        this.unitNumber = unitNumber;
        return this;
    }

    /**
     * @return unit of given price
     */
    public final UnitType getUnitType() {
        return unitType;
    }

    /**
     *
     * @param unitType
     *            unit of given price
     * @return this instance for chaining
     */
    public final UnitPrice setUnitType(final UnitType unitType) {
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
    public final UnitPrice setDescription(final String description) {
        this.description = description;
        return this;
    }

    @Override
    @JsonIgnore
    public final UnitPrice getValid() {
        return ValidationUtils.getValid(this, amountWithVat, netAmount, netAmountEur, netAmountNational);
    }
}
